{-# LANGUAGE CPP #-}
module NVT.Driver where

import NVT.CUDASDK
import NVT.Diagnostic
import NVT.ElfDecode
import NVT.FilterAssembly
import NVT.ListingTypes
import NVT.Opts
import NVT.RawInst
import NVT.Parsers.ListingParser
import NVT.SASSGraph

import           Prog.Args.Args((#))-- progargs
import qualified Prog.Args.Args as PA -- progargs

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Debug.Trace
import System.FilePath
import System.Directory
import System.Process
import System.Exit
import System.Environment(lookupEnv)
import System.IO
import Text.Printf
import qualified Data.ByteString as S

-------------------------------------------------------------------------------
--    .cu -> .cubin -> .ptx -> .sass     nvcc -cubin =>
--                     .ptx -> .sass
--           .cubin ->      -> .sass
--    .cl           -> .ptx -> .sass
spec :: PA.Spec Opts
spec = PA.mkSpecWithHelpOpt "nva" ("NVidia Assembly Translator " ++ nvt_version) 0
    [ -- options
      PA.opt spec "a" "arch" "ARCH"
        "sets the device architecture (e.g. -a=sm_72)" ""
        (\a o -> (o {oArch = a})) # PA.OptAttrAllowUnset
    , PA.optF spec "lines" "print-line-info"
        "enables line mappings" ""
        (\o -> (o {oPrintLines = True})) # PA.OptAttrAllowUnset
    , PA.optF spec "lineinfo" "generate-line-info"
        "same as -lines" "for nvcc compatibility"
        (\o -> (o {oPrintLines = True})) # PA.OptAttrAllowUnset
    , collate_opt
    , color_opt
    , PA.opt spec "I" "include-path" "PATH"
        "add -I option to nvcc (careful to use =; -I=../path)"
        "same effect as -X=-I../path"
        (\a o -> (o {oIncludePaths = oIncludePaths o ++ [a]})) # PA.OptAttrAllowUnset # PA.OptAttrAllowMultiple
    , PA.optF spec "" "no-filter-asm"
        "does not filter assembly code" ""
        (\o -> (o {oFilterAssembly = False})) # PA.OptAttrAllowUnset
    , PA.optF spec "ndeps" "no-print-deps"
        "do not print dependencies" ""
        (\o -> (o {oPrintDeps = False})) # PA.OptAttrAllowUnset
    , PA.optF spec "hex" "print-instruction-encoding"
        "prints instruction encodings" ""
        (\o -> (o {oPrintEncoding = True})) # PA.OptAttrAllowUnset
    , PA.optF spec "" "override-ptx-bitsize"
        "overrides .address_size 32 directive to omit the proper -m32 in nvcc (workaround for line number issues)" ""
        (\o -> (o {oOverridePtxAddrSize32 = True})) # PA.OptAttrAllowUnset
    , PA.opt spec "o" "output" "PATH"
        "sets the output file" "(defaults to stdout)"
        (\f o -> (o {oOutputFile = f})) # PA.OptAttrAllowUnset
    , PA.optF spec "offs" "print-offsets"
        "print section offsets in SASS" ""
        (\o -> (o {oPrintOffsets = True})) # PA.OptAttrAllowUnset

    , PA.optF spec "rdc" "relocatable-device-code"
        "pass -rdc=true to nvcc" ""
        (\o -> (o {oRDC = True})) # PA.OptAttrAllowUnset
    , PA.opt spec "" "save-cubin" "PATH"
        "saves the intermediate .cubin file to this path" ""
        (\f o -> (o {oSaveCuBin = f})) # PA.OptAttrAllowUnset
    , PA.optF spec "ptx" "save-ptx"
        "saves the intermediate .ptx file (based on input file)" ""
        (\o -> (o {oSavePtx = True})) # PA.OptAttrAllowUnset
    , PA.opt spec "" "save-ptx-to" "PATH"
        ("enables and specifies non-default path for ptx") ""
        (\f o -> (o {oSavePtxTo = f, oSavePtx = True})) # PA.OptAttrAllowUnset
    , PA.optF spec "text" "print-code"
        "print text sections only" "via nvdiasm --print-code"
        (\o -> (o {oTextSectionsOnly = True})) # PA.OptAttrAllowUnset
    , PA.opt spec "Xcl2ptx" "" "ANYTHING"
        "sets an extra argument for the cl2ptx tool" "(e.g. -Xcl2ptx=-cl-fp32-correctly-rounded-divide-sqrt)"
        (\a o -> (o {oExtraCl2ptxArgs = oExtraCl2ptxArgs o ++ [a]})) # PA.OptAttrAllowUnset # PA.OptAttrAllowMultiple
    , PA.opt spec "Xnvcc" "" "ANYTHING"
        "sets an extra argument for the nvcc tool" "(e.g. -Xnvcc=-maxregcount -X=64; note how they are successive)"
        (\a o -> (o {oExtraNvccArgs = oExtraNvccArgs o ++ [a]})) # PA.OptAttrAllowUnset # PA.OptAttrAllowMultiple
    , PA.opt spec "Xnvdisasm" "" "ANYTHING"
        "sets an extra argument for the nvdiasm tool" ""
        (\a o -> (o {oExtraNvdisasmArgs = oExtraNvdisasmArgs o ++ [a]})) # PA.OptAttrAllowUnset # PA.OptAttrAllowMultiple
    , PA.optF spec "v2" "debug"
        "the verbosity level" ""
        (\o -> (o {oVerbosity = 2})) # PA.OptAttrAllowUnset
    , PA.optF spec "v" "verbose"
        "the verbosity level" ""
        (\o -> (o {oVerbosity = 1})) # PA.OptAttrAllowUnset
    ]
    [ -- arguments
        PA.arg spec "PATH"
          "The file to read from" ""
          (\f o -> (o {oInputFile = f})) # PA.OptAttrAllowUnset
    ]
  where collate_opt =
          PA.optVFIO spec "c" "collate" "[aps]"
            "collate listing by src line (implies -lines)"
            ("  'a'/'all' implies all intermediate forms\n" ++
             "  'p'/'ptx' implies to map PTX lines\n" ++
             "  's'/'sass' implies to map SASS lines\n")
            (\o -> return o {oCollateListing = [CollateSASS]})
            parseCollateOptValue # PA.OptAttrAllowUnset

        parseCollateOptValue :: String -> Opts -> IO Opts
        parseCollateOptValue s o = do
          let parseTk tk = do
                case tk of
                  "a" -> return [CollatePTX,CollateSASS]
                  "all" -> return [CollatePTX,CollateSASS]
                  "p" -> return [CollatePTX]
                  "ptx" -> return [CollatePTX]
                  "s" -> return [CollateSASS]
                  "sass" -> return [CollateSASS]
                  _ -> fatal $ "-c=" ++ s ++ ": " ++ tk ++ ": invalid value"

          let tokenizeOnCommas = words . map (\c -> if c == ',' then ' ' else c)
          cs <- nub . concat <$> mapM parseTk (tokenizeOnCommas s)
          return o {oCollateListing = cs}

        color_opt =
          PA.optPIO spec "" "color" "CMODE"
            "set options to enable output coloring (always|never|auto)"
            "The 'auto' option uses color if the output is a tty."
            parseColor # PA.OptAttrAllowUnset

        -- parseColor :: Spec a => String -> Opts -> IO Opts
        parseColor _ inp os =
          case inp of
            "always" -> return os{oColor = ColorAlways}
            "never" -> return os{oColor = ColorNever}
            "auto" -> return os{oColor = ColorAuto}
            "tty" -> return os{oColor = ColorAuto}
            _ -> fatal $ "--color=" ++ inp ++ ": invalid color value"

nvt_version :: String
nvt_version = "1.1.8" -- using -print-line-info-inline
-- nvt_version = "1.1.7"


run :: [String] -> IO ()
run as = PA.parseArgs spec dft_opts as >>= runWithOpts


getTemporaryFile :: String -> IO FilePath
getTemporaryFile sfx = try 0
  where try :: Int -> IO FilePath
        try n = do
          let fp = printf "%02d-" ++ sfx
          z <- doesFileExist fp
          if z then try (n+1)
            else return fp

runWithOpts :: Opts -> IO ()
runWithOpts os
  | null (oInputFile os) = getContents >>= processStdin (oStdinIs os)
  | otherwise = processFile (oInputFile os)
  where processStdin :: StdinIs -> String -> IO ()
        processStdin stdin_fmt stdin_str
          | stdin_fmt == StdinIsUnknown =
              case inferStdin stdin_str of
                StdinIsUnknown -> fatal "cannot infer stdin format"
                x -> handleInpFmt x
          | otherwise = handleInpFmt stdin_fmt
          where handleInpFmt fmt =
                  case fmt of
                    StdinIsCu -> useTempFile "default.cu"
                    StdinIsPtx -> useTempFile "default.ptx"
                    StdinIsSass -> processSassToOutput "" stdin_str

                useTempFile fp_sfx = do
                 tmp_fp <- getTemporaryFile fp_sfx
                 writeFile tmp_fp stdin_str
                 runWithOpts (os{oInputFile = tmp_fp})
                 removeFile tmp_fp

        -- given a .cu input, this gives the stem for .ptx or .cubin
        deriveFileName :: String -> FilePath
        deriveFileName = deriveFileNameFrom (oInputFile os)
        deriveFileNameFrom :: FilePath -> String -> FilePath
        deriveFileNameFrom from ext
          | null from = "default" ++ ext
          | otherwise = takeFileName (dropExtension from) ++ ext

        inferStdin :: String -> StdinIs
        inferStdin stdin_str
          | any (".headerflags"`isInfixOf`) pfx_lns = StdinIsSass
          | any (".version"`isPrefixOf`) pfx_lns = StdinIsPtx
          | any (".target"`isPrefixOf`) pfx_lns = StdinIsPtx
          | any ("#include"`isPrefixOf`) pfx_lns = StdinIsCu
          | any ("#define"`isPrefixOf`) pfx_lns = StdinIsCu
          | any ("__kernel"`isPrefixOf`) pfx_lns = StdinIsCl
          | otherwise = StdinIsUnknown
          where pfx_lns = take 32 (lines stdin_str)

        processFile :: FilePath -> IO ()
        processFile fp = do
          debugLn os $ show os
          case takeExtension fp of
            ".cl" -> processClFile fp
            ".cu" -> processCuFile fp []
            ".cubin" -> processCubinFile fp
            ".ptx" -> processPtxFile fp  -- (use NVCC)
            ".sass" -> processSassFile fp
          -- TODO: support .nva (ELF -> .cubin)
          -- TODO: figure out how to turn a .cubin into a .obj or .exe and run it
          --       is there an API to load a .cubin with?
            _ -> fatal (takeFileName fp ++ ": unable to handle file by extension")

        processPtxFile :: FilePath -> IO ()
        processPtxFile ptx_fp = do
          -- .address_size 32
          let isA32Line ln =
                case words ln of
                  [".address_size","32"] -> True
                  _ -> False
          fstr <- readFile' ptx_fp
          let has_a32 = any isA32Line (lines fstr)
          let a32_opt
                | not (oOverridePtxAddrSize32 os) && has_a32 = ["-m32"]
                | otherwise = []
          processCuFile ptx_fp a32_opt

        processClFile :: FilePath -> IO ()
        processClFile cl_fp = do
          (ec,_,_) <- runProcessWithExitCode (mkExe "cl2ptx") ["--find-compiler"]
          case ec of
            ExitFailure err -> fatal $ "could not find cl2ptx (https://gitlab.com/trbauer/cl2ptx)"
            ExitSuccess -> return ()
          when (null (oArch os)) $
            fatal $ cl_fp ++ ": requires --arch argument"
          z <- doesFileExist cl_fp
          when (not z) $
            fatal $ cl_fp ++ ": file not found"
          let needs_lines = oPrintLines os || not (null (oCollateListing os))
          let (ptx_dst,del_ptx)
                | not (null (oSavePtxTo os)) = (oSavePtxTo os,False)
                | oSavePtx os = (deriveFileNameFrom cl_fp ".ptx",False)
                | otherwise = ("nva-" ++ deriveFileNameFrom cl_fp ".ptx",True)
              build_opts =
                  "-cl-nv-arch " ++ oArch os ++ maybe_clstd ++ maybe_lines
                where maybe_lines = if oPrintLines os then " -nv-line-info" else ""
                      maybe_clstd
                        | any hasClstd (oExtraCl2ptxArgs os) = ""
                        | otherwise = " -cl-nv-cstd=CL1.2"
                        where hasClstd x = "-b="`isPrefixOf`x && "-cl-nv-cstd="`isInfixOf`x
              cl2ptx_args =
                [cl_fp, "-b=" ++ build_opts,"-o=" ++ ptx_dst] ++ oExtraCl2ptxArgs os
          (ec,out,err) <- runProcessWithExitCode (mkExe "cl2ptx") cl2ptx_args
          case ec of
            ExitFailure e ->
              fatal $
                "cl2ptx: exited " ++ show e ++ "\n" ++
                err ++ (if null out then "" else "\n") ++ out
            ExitSuccess -> return ()
          when needs_lines $
            fixDotFileDirectiveInPtxForOpenCL ptx_dst cl_fp
          runWithOpts os {oInputFile = ptx_dst}
          when del_ptx $ removeFile ptx_dst

        readFile' :: FilePath -> IO String
        readFile' fp = do
          fstr <- readFile fp
          length fstr `seq` return ()
          return fstr

        runProcessWithExitCode :: FilePath -> [String] -> IO (ExitCode,String,String)
        runProcessWithExitCode exe args = do
          let escArg a = if any isSpace a then show a else a
          verboseLn os $ "% " ++ exe ++ "^\n" ++
            concatMap (\a -> "   " ++ escArg a ++ "\n") args
          r@(ec,out,err) <- readProcessWithExitCode exe args ""
          verboseLn os ("  ==> " ++ show ec)
          debugLn os err
          debugLn os out
          return r

        -- foo.cu --> $temp.cubin and disassembles it that file
        processCuFile :: FilePath -> [String] -> IO ()
        processCuFile fp maybe_bitsize_opt = do
          when (null (oArch os)) $
            fatal $ fp ++ ": requires --arch argument"
          z <- doesFileExist fp
          when (not z) $
            fatal $ fp ++ ": file not found"
          cl_bin_dir <- findExtraNvccOpts os
          --
          -- cs <- findCudaSamplesDir
          -- let cuda_sample_incs_dir = if null cs then [] else  ["-I" ++ cs]
          let needs_lines = oPrintLines os || not (null (oCollateListing os))

              mkNvccArgs targ =
                ["-arch", oArch os, targ] ++
                maybe_bitsize_opt ++
                map (\i -> "-I" ++ i) (oIncludePaths os) ++
                cl_bin_dir ++
                (if needs_lines then ["-lineinfo"] else []) ++
                maybeOpt oRDC "-rdc=true" ++
                oExtraNvccArgs os ++
                -- cuda_sample_incs_dir ++
                [oInputFile os]
          out <- runCudaTool os "nvcc" (mkNvccArgs "-cubin")
          verboseLn os out
          let cubin_file = deriveFileName ".cubin"
              output_path_without_ext
                | null (oOutputFile os) = takeFileName (dropExtension fp)
                | otherwise = dropExtension (oOutputFile os)
              ptx_implicit_output = deriveFileNameFrom fp ".ptx"
              ptx_required =
                   oSavePtx os
                || not (null (oSavePtxTo os))
                || CollatePTX `elem` oCollateListing os
              ptx_dst
                | null (oSavePtxTo os) = deriveFileNameFrom fp ".ptx"
                | otherwise = oSavePtxTo os
          --
          ptx_src <-
            if ".ptx"`isSuffixOf`fp then readFile' fp -- input was .ptx
              else if not ptx_required then return "" -- input was .cu, but ptx isn't needed
                else do -- have to re-run since -ptx and -cubin conflict
                  _ <- runCudaTool os "nvcc" (mkNvccArgs "-ptx")
                  when (ptx_implicit_output /= ptx_dst) $ do
                    debugLn os $
                      "copying\n" ++
                      "   from " ++ ptx_implicit_output ++ "\n" ++
                      "   to " ++ ptx_dst
                    renameFile ptx_implicit_output ptx_dst
                  ptx <- readFile ptx_dst
                  length ptx `seq` return ()
                  when (not (oSavePtx os)) $
                    removeFile ptx_dst
                  return ptx
          --
          -- .cu ==> .cubin => .sass
          --                ^^ here
          processCubinFileWithPtx ptx_src cubin_file
          --
          if null (oSaveCuBin os) then do
              debugLn os $ cubin_file ++ ": removing"
              -- removeFile cubin_file
            else do
              debugLn os $
                cubin_file ++ ": renaming to " ++ output_path_without_ext ++ ".cubin"
              renameFile cubin_file (output_path_without_ext ++ ".cubin")

        processSassFile :: FilePath -> IO ()
        processSassFile sass_fp
          | oFilterAssembly os = readFile sass_fp >>= processSassToOutput ""
          | otherwise = do
            fstr <- readFile sass_fp
            case parseListing sass_fp fstr of
              Left err -> do
                -- hPutStrLn stderr (dFormatWithLines (lines sass_fp) d)
                fatal (dFormatWithLines (lines sass_fp) err)
                return ()
              Right l -> do
                {-
                let withOutput : String -> IO ()
                    withOutput str
                      | null (oOutputFile os) = hPutStr stdout
                      | otherwise = withFile (oOutputFile os) WriteMode

                      | withFile (oOutputFile os) WriteMode
                withOutput $ fmtListingGraph l
                -}
                mapM_ emitTextSection (lTextSections l)

        processSassToOutput :: String -> String -> IO ()
        processSassToOutput ptx sass
          | null (oOutputFile os) =
            processSassIO stdout ptx sass
          | otherwise =
            withFile (oOutputFile os) WriteMode $ \h -> processSassIO h ptx sass

        processSassIO :: Handle -> String -> String -> IO ()
        processSassIO h_out ptx sass
          | null (oArch os) = fatal "--arch required for this mode"
          | otherwise = do
            is_tty <- hIsTerminalDevice h_out
            let color = oColor os == ColorAlways || oColor os == ColorAuto && is_tty
                fos = fos_dft {
                          foArch = oArch os
                        , foColor = color
                        , foFmtOpts =
                            (foFmtOpts fos_dft) {
                              foPrintEncoding = oPrintEncoding os
                            , foPrintOffsets = oPrintOffsets os
                            , foPrintDeps = oPrintDeps os
                            }
                        , foVerbosity = oVerbosity os
                        }
            if not (null (oCollateListing os))
              then do
                when (oVerbosity os >= 2) $ do
                  unless (null ptx) $
                    writeFile "debug-collate.ptx" ptx
                  unless (null sass) $
                    writeFile "debug-collate.sass" sass
                let c_ptx = if CollatePTX `elem` oCollateListing os then ptx else ""
                let c_sass = if CollateSASS `elem` oCollateListing os then sass else ""
                emitCollatedListing fos h_out c_ptx c_sass
              else if not (oFilterAssembly os) then hPutStr h_out sass
              else filterAssemblyWithInterleavedSrcIO fos h_out sass

        emitOutput :: String -> IO ()
        emitOutput
          | null (oOutputFile os) = putStr
          | otherwise = writeFile (oOutputFile os)
        appendOutput :: String -> IO ()
        appendOutput
          | null (oOutputFile os) = putStr
          | otherwise = appendFile (oOutputFile os)

        processCubinFile :: FilePath -> IO ()
        processCubinFile = processCubinFileWithPtx ""
        processCubinFileWithPtx :: String -> FilePath -> IO ()
        processCubinFileWithPtx maybe_ptx cubin_file = do
          -- let nv_cuod_args = ["--dump-sass", cubin_file]
          -- cuod_out <- runCudaTool os "cuobjdump" nv_cuod_args
          let nvdis_args_no_lines =
                maybeOpt oTextSectionsOnly "--print-code" ++
                [
                  "--no-vliw" -- disables the {...}
                , "--print-instruction-encoding"
                , "--no-dataflow"
                , cubin_file
                ] ++ oExtraNvdisasmArgs os

          let tryNvdisasmWithoutLineNumbers :: SomeException -> IO String
              tryNvdisasmWithoutLineNumbers e = do
                warningLn os "nvdisasm: failed trying with*out* --print-line-info"
                runCudaTool os "nvdisasm" nvdis_args_no_lines
          --
          let args_lines = ["--print-line-info","--print-line-info-inline"]
          nvdis_out <- runCudaTool os "nvdisasm" (args_lines ++ nvdis_args_no_lines)
            `catch` tryNvdisasmWithoutLineNumbers
          processSassToOutput maybe_ptx nvdis_out
          --
          -- cuod_res <- runCudaTool os "cuobjdump" ["--dump-resource-usage", cubin_file]
          -- appendOutput cuod_res

        -- UNREACHABLE for the moment
        processCubinFileNative :: FilePath -> IO ()
        processCubinFileNative fp = do
          bs <- S.readFile fp
          decodeElf dft_edo bs
          return ()

        maybeOpt f tk
          | f os = [tk]
          | otherwise = []


--  .file   1 "E:\\dev\\nvaa\\<unknown>"
--  .file   2 "E:\\dev\\nvaa\\<kernel>"
-- e.g.
-- .file  2 "D:\\work\\projects\\new-eu\\setup\\<kernel>"
-- ==>
-- .file  2 "D:\\work\\projects\\new-eu\\setup\\foo.cl"
fixDotFileDirectiveInPtxForOpenCL :: FilePath -> FilePath ->  IO ()
fixDotFileDirectiveInPtxForOpenCL ptx_fp cl_fp = do
  flns <- lines <$> readFile ptx_fp
  length flns `seq` return () -- close handle so we can re-write it
  let is_abs = isAbsolute cl_fp
  let fixLine ln
        | is_abs && ".file"`isInfixOf`ln && "<kernel>\""`isSuffixOf`ln =
          case words ln of
            ".file":f_ix:sfx -> "\t.file  " ++ f_ix ++ "  " ++ show cl_fp
            _ -> fix ln
        | ".file"`isInfixOf`ln = fix ln
        | otherwise = ln
        where fix [] = []
              fix s@(c:cs)
                | "<kernel>\""`isPrefixOf`s = drop 1 (show cl_fp) ++ drop (length "<kernel>\"") s
                | otherwise = c : fix cs
  writeFile ptx_fp $
    unlines (map fixLine flns)
