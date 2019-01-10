{-# LANGUAGE CPP #-}
module NVT.Driver where

import NVT.FilterAssembly

import Control.Exception
import Control.Monad
import Data.List
import System.FilePath
import System.Directory
import System.Process
import System.Exit
import System.Environment(lookupEnv)

import qualified Data.ByteString as S

-- import Language.NVDA.IR
-- import Language.NVDA.Parser

import NVT.ElfDecode
import           Prog.Args.Args((#))-- progargs
import qualified Prog.Args.Args as PA -- progargs

data Opts =
  Opts {
    oVerbosity :: !Int
  , oArch :: !String -- e.g. sm_52, sm_75
  , oOutputFile :: !FilePath
  , oSaveCuBin :: !FilePath -- if input is .cu, this is the .cubin
  , oSavePtx :: !FilePath
--  , oUseCuobjdump :: !Bool -- versus nvdisasm
  , oInputFile :: !FilePath
  , oSourceMapping :: !Bool
  , oFilterAssembly :: !Bool
  , oRDC :: !Bool
  , oExtraArgs :: [String]
  } deriving Show
dft_opts :: Opts
dft_opts =
  Opts {
    oVerbosity = 0
  , oArch = "" -- "sm_62" -- "sm_70" -- "sm_52"
  , oOutputFile = ""
  , oSaveCuBin = ""
  , oSavePtx = ""
  , oInputFile = ""
--  , oUseCuobjdump = False
  , oSourceMapping = False
  , oFilterAssembly = True
  , oRDC = False
  , oExtraArgs = []
  }
dft_opts_75 :: Opts
dft_opts_75 = dft_opts{oArch = "sm_75"}

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
            , PA.optF spec "rdc" "--relocatable-device-code"
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

--------------
debugLn :: Opts -> String -> IO ()
debugLn os
  | oVerbosity os > 1 = putStrLn
  | otherwise = const (return ())
printLn :: Opts -> String -> IO ()
printLn os
  | oVerbosity os >= 0 = putStrLn
  | otherwise = const (return ())
warningLn :: Opts -> String -> IO ()
warningLn os
  | oVerbosity os >= -1 = putStrLn
  | otherwise = const (return ())

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
          cl_bin_dir <- findClBinDir
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
          runCudaTool "nvcc" (mkArgs "-cubin")
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
            runCudaTool "nvcc" (mkArgs "-ptx")
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

        runCudaTool :: String -> [String] -> IO String
        runCudaTool tool args = do
          tool_exe <- findCudaTool os (mkExe tool)
          -- debugLn os $ show tool_exe ++ " " ++ show args
          runProcess tool_exe args

        runProcess :: FilePath -> [String] -> IO String
        runProcess exe args = do
          debugLn os $ "% " ++ exe ++ "\n" ++ concatMap (\a -> "      "++a ++ "\n") args
          (ec,out,err) <- readProcessWithExitCode exe args ""
          debugLn os $ "% " ++ show ec
          case ec of
            ExitFailure ex_val -> do
              let lbl = "[" ++ dropExtension (takeFileName exe) ++ "] "
              fatal $ labelLines lbl $
                err ++ out ++ "\n" ++ "[exited " ++ show ex_val ++ "]"
            ExitSuccess -> return out

        findClBinDir :: IO [String]
#ifndef mingw32_HOST_OS
        findClBinDir = return []
#else
        -- C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\INCLUDE
        findClBinDir = tryDir ["C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC"]
          where tryDir [] = return []
                tryDir (d:ds) = do
                  z <- doesDirectoryExist (d ++ "\\BIN")
                  if not z then tryDir ds
                    else do
                      nvcc_exe <- findCudaTool os "nvcc"
                      let cuda_include_dir = takeDirectory (takeDirectory nvcc_exe) ++ "\\include"
                      z <- doesDirectoryExist cuda_include_dir
                      unless z $
                        die "can't find CUDA include directory"
                      return
                        [
                          "--compiler-bindir", d
                        -- TODO: need to look these up
                        -- , "-IC:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v9.0\\include"
                        , "-I" ++ cuda_include_dir
                        -- , "-IC:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\INCLUDE"
                        , "-I" ++ d ++ "\\INCLUDE"
                        -- TODO: need to lookup
                        , "-IC:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.15063.0\\ucrt"
                        ]
-- pick the newest child of:
--   C:\Program Files (x86)\Windows Kits\10\include
#endif
        processCubinFile :: FilePath -> IO ()
        processCubinFile cubin_file = do
          -- let nv_cuod_args = ["--dump-sass", cubin_file]
          -- cuod_out <- runCudaTool "cuobjdump" nv_cuod_args
          let nvdis_args_no_lines =
                [
                  "--no-vliw" -- disables the {...}
                -- , "--print-code" -- print program text sections only
                , "--print-instruction-encoding"
                , "--no-dataflow"
                , cubin_file
                ]
          let tryNvdisasmWithoutLineNumbers :: SomeException -> IO String
              tryNvdisasmWithoutLineNumbers e = do
                warningLn os "nvdisasm: failed trying with*out* --print-line-info"
                runCudaTool "nvdisasm" nvdis_args_no_lines
          --
          nvdis_out <- runCudaTool "nvdisasm" (["--print-line-info"] ++ nvdis_args_no_lines)
            `catch` tryNvdisasmWithoutLineNumbers
          cuod_res <- runCudaTool "cuobjdump" ["--dump-resource-usage", cubin_file]
          let filterAsm
                | oFilterAssembly os = filterAssembly (oArch os)
                | otherwise = id
          emitOutput (filterAsm nvdis_out ++ cuod_res)

        processCubinFile2 :: FilePath -> IO ()
        processCubinFile2 fp = do
          bs <- S.readFile fp
          decodeElf dft_edo bs
          return ()

        maybeOpt f tk
          | f os = [tk]
          | otherwise = []


-- setupEnv :: IO [(String,String)]
-- setupEnv =
-- DevEnvDir=C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\
-- INCLUDE=
--   C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\INCLUDE;
--   C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\ATLMFC\INCLUDE;
--   C:\Program Files (x86)\Windows Kits\10\include\10.0.14393.0\ucrt;
--   C:\Program Files (x86)\Windows Kits\NETFXSDK\4.6.1\include\um;
--   C:\Program Files (x86)\Windows Kits\10\include\10.0.14393.0\shared;
--   C:\Program Files (x86)\Windows Kits\10\include\10.0.14393.0\um;
--   C:\Program Files (x86)\Windows Kits\10\include\10.0.14393.0\winrt;

labelLines :: String -> String -> String
labelLines pfx = unlines . map (pfx++) . lines

mkExe :: FilePath -> FilePath
#ifdef mingw32_HOST_OS
mkExe fp
  | ".exe" `isSuffixOf` fp = fp
  | otherwise = fp ++ ".exe"
#else
mkExe fp = fp
#endif

------- finds a CUDA executable
findCudaTool :: Opts -> String -> IO FilePath
findCudaTool os exe_raw =
    tryEnvs
      [
        "CUDA_PATH"
      , "CUDA_PATH_V10_0"
      , "CUDA_PATH_V9_0"
      , "CUDA_PATH_V9_1"
      , "CUDA_PATH_V8_0"
      , "CUDA_PATH_V7_5"
      ]
  where exe = mkExe exe_raw

        tryEnvs (e:es) = do
          mv <- lookupEnv e
          case mv of
            Nothing -> tryEnvs es
            Just d -> do
              tryPath (d </> "bin" </> exe) (tryEnvs es)
#ifdef mingw32_HOST_OS
        tryEnvs [] =
          tryFixedPaths (map ("C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\"++) vers)
            where vers = ["v10.0","v9.1","v9.0","v8.0","v7.5"]
#else
        -- TODO: /opt/nvidia/nvcc ?
        tryEnvs [] = tryFixedPaths ["/usr/local/cuda/bin"]
#endif
        tryFixedPaths (d:ds) = do
          tryPath (d </> "bin" </> exe) (tryFixedPaths ds)
        tryFixedPaths [] = giveUp

        tryPath p keep_looking = do
          z <- doesFileExist p
          if not z then keep_looking else do
            debugLn os $ "findCudaToolDir: found " ++ p
            return p

        giveUp :: IO FilePath
        giveUp = do
          warningLn os $ "findCudaToolDir: falling back to $PATH prefix (could not find $CUDA_PATH)"
          return exe

-- find VS

fatal :: String -> IO a
fatal msg = die msg


-- "C:\\ProgramData\\NVIDIA Corporation\\CUDA Samples\\v8.0\\0_Simple"

getSubPaths :: FilePath -> IO [FilePath]
getSubPaths dir =
  reverse .
    map (dir </>) .
      filter ((/=".") . take 1) <$> getDirectoryContents dir


