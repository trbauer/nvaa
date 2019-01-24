{-# LANGUAGE CPP #-}
module NVT.Driver where

import NVT.CUDASDK
import NVT.ElfDecode
import NVT.FilterAssembly
import NVT.Opts

import           Prog.Args.Args((#))-- progargs
import qualified Prog.Args.Args as PA -- progargs

import Control.Exception
import Control.Monad
import Data.List
import System.FilePath
import System.Directory
import System.Process
import System.Exit
import System.Environment(lookupEnv)
import qualified Data.ByteString as S

-------------------------------------------------------------------------------
--    .cu -> .cubin -> .ptx -> .sass     nvcc -cubin =>
--                     .ptx -> .sass
--           .cubin ->      -> .sass
spec :: PA.Spec Opts
spec = PA.mkSpecWithHelpOpt "nvt" ("NVidia Translator " ++ nvt_version) 0
            [ -- options
              PA.optF spec "v" "verbose"
                "the verbosity level" ""
                (\o -> (o {oVerbosity = 1})) # PA.OptAttrAllowUnset
            , PA.optF spec "v2" "debug"
                "the verbosity level" ""
                (\o -> (o {oVerbosity = 2})) # PA.OptAttrAllowUnset
            , PA.optF spec "" "line-mappings"
                "enables line mappings" ""
                (\o -> (o {oSourceMapping = True})) # PA.OptAttrAllowUnset
            , PA.optF spec "" "no-filter-asm"
                "does not filter assembly code" ""
                (\o -> (o {oFilterAssembly = False})) # PA.OptAttrAllowUnset
            , PA.optF spec "rdc" "relocatable-device-code"
                "pass -rdc=true to nvcc" ""
                (\o -> (o {oRDC = True})) # PA.OptAttrAllowUnset
            , PA.opt spec "o" "output" "PATH"
                "sets the output file" "(defaults to stdout)"
                (\f o -> (o {oOutputFile = f})) # PA.OptAttrAllowUnset
            , PA.opt spec "a" "arch" "ARCH"
                "sets the device architecture (e.g. -a=sm_72)" ""
                (\a o -> (o {oArch = a}))

            , PA.opt spec "" "save-cubin" "PATH"
                "saves the intermediate .cubin file to this path" ""
                (\f o -> (o {oSaveCuBin = f})) # PA.OptAttrAllowUnset
            , PA.opt spec "" "save-ptx" "PATH"
                "saves the intermediate .ptx file to this path" ""
                (\f o -> (o {oSavePtx = f})) # PA.OptAttrAllowUnset

            , PA.opt spec "X" "" "ANYTHING"
                "sets an extra argument for the nvcc tool (e.g. -X-Ic:\\foo\\bar)" ""
                (\a o -> (o {oExtraArgs = oExtraArgs o ++ [a]})) # PA.OptAttrAllowUnset
            ]
            [ -- arguments
                PA.arg spec "PATH"
                  "The file to read from" ""
                  (\f o -> (o {oInputFile = f}))
            ]
nvt_version :: String
nvt_version = "1.0.0"



run :: [String] -> IO ()
run as = PA.parseArgs spec dft_opts as >>= runWithOpts


-- how to inject SASS?
-- http://stackoverflow.com/questions/20012318/how-to-compile-ptx-code


-- information on the current machine
{-
data EnvInfo =
  EnvInfo {
    eiSamplesPath :: Maybe FilePath
  , eiSdkRoot :: Maybe FilePath
  , eiPathToCpp :: Maybe FilePath
  } deriving (Show,Eq)

findEnvInfo :: Opts -> IO EnvInfo
findEnvInfo = ...
-}

runWithOpts :: Opts -> IO ()
runWithOpts os = processFile (oInputFile os)
  where processFile :: FilePath -> IO ()
        processFile fp = do
          debugLn os $ show os
          case takeExtension fp of
            ".cu" -> processCuFile fp
            ".cubin" -> processCubinFile fp
            ".ptx" -> processPtxFile fp  -- (use NVCC)
            ".sass" -> processSassFile fp
          -- TODO: support .ptx (ptx -> .cubin -> .nva)
          -- TODO: support .nva (ELF -> .cubin)
          -- TODO: figure out how to turn a .cubin into a .obj or .exe and run it
          --       is there an API to load a .cubin with?
            _ -> fatal (takeFileName fp ++ ": unable to handle file by extension")

        processPtxFile :: FilePath -> IO ()
        processPtxFile = processCuFile

        -- foo.cu --> $temp.cubin and disassembles it that file
        processCuFile :: FilePath -> IO ()
        processCuFile fp = do
          z <- doesFileExist fp
          when (not z) $
            fatal $ fp ++ ": file not found"
          cl_bin_dir <- findExtraNvccOpts os
          --
          -- cs <- findCudaSamplesDir
          -- let cuda_sample_incs_dir = if null cs then [] else  ["-I" ++ cs]
          let mkArgs targ =
                ["-arch",oArch os,targ] ++
                cl_bin_dir ++
                maybeOpt oSourceMapping "-lineinfo" ++
                maybeOpt oRDC "-rdc=true" ++
                oExtraArgs os ++
                -- cuda_sample_incs_dir ++
                [oInputFile os]
          runCudaTool os "nvcc" (mkArgs "-cubin")
          let cubin_file = takeFileName (dropExtension fp) ++ ".cubin"
          let output_path_without_ext
                | null (oOutputFile os) = takeFileName (dropExtension fp)
                | otherwise = dropExtension (oOutputFile os)
          --
          unless (null (oSaveCuBin os)) $ do
            -- putStrLn "copying cubin"
            bs <- S.readFile cubin_file
            S.writeFile (output_path_without_ext ++ ".cubin") bs
          --
          unless (null (oSavePtx os)) $ do
            runCudaTool os "nvcc" (mkArgs "-ptx")
            --
            let ptx_file = takeFileName (dropExtension fp) ++ ".ptx"
            -- putStrLn $
            --   "copying\n" ++
            --   "   from " ++ ptx_file ++ "\n" ++
            --   "   to " ++ oSavePtx os
            bs <- S.readFile ptx_file
            S.writeFile (oSavePtx os) bs
            removeFile ptx_file
          --
          processCubinFile cubin_file
          --
          removeFile cubin_file

        processSassFile :: FilePath -> IO ()
        processSassFile fp = error "processSassFile: todo"

        emitOutput :: String -> IO ()
        emitOutput
          | null (oOutputFile os) = putStr
          | otherwise = writeFile (oOutputFile os)
        appendOutput :: String -> IO ()
        appendOutput
          | null (oOutputFile os) = putStr
          | otherwise = appendFile (oOutputFile os)

        processCubinFile :: FilePath -> IO ()
        processCubinFile cubin_file = do
          -- let nv_cuod_args = ["--dump-sass", cubin_file]
          -- cuod_out <- runCudaTool os "cuobjdump" nv_cuod_args
          let nvdis_args_no_lines =
                maybeOpt oTextOnly "--print-code" ++
                [
                  "--no-vliw" -- disables the {...}
                , "--print-instruction-encoding"
                , "--no-dataflow"
                , cubin_file
                ]
          let tryNvdisasmWithoutLineNumbers :: SomeException -> IO String
              tryNvdisasmWithoutLineNumbers e = do
                warningLn os "nvdisasm: failed trying with*out* --print-line-info"
                runCudaTool os "nvdisasm" nvdis_args_no_lines
          --
          nvdis_out <- runCudaTool os "nvdisasm" (["--print-line-info"] ++ nvdis_args_no_lines)
            `catch` tryNvdisasmWithoutLineNumbers
          let filterAsm
                | oFilterAssembly os = filterAssembly (oArch os)
                | otherwise = id
          emitOutput (filterAsm nvdis_out)
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
