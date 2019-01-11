{-# LANGUAGE CPP #-}
module NVT.CUDASDK where

import NVT.Opts

import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import System.Directory
import System.Environment(lookupEnv)
import System.Exit
import System.FilePath
import System.Process


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
-- find VS
-- "C:\\ProgramData\\NVIDIA Corporation\\CUDA Samples\\v8.0\\0_Simple"


-- finds extra cl includes
findExtraNvccOpts :: Opts -> IO [String]
#ifndef mingw32_HOST_OS
findExtraNvccOpts _ = return []
#else
-- C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\INCLUDE
findExtraNvccOpts os =
    tryDir ["C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC"]
  where tryDir [] = return []
        tryDir (vs_dir:ds) = do
          z <- doesDirectoryExist (vs_dir ++ "\\BIN")
          if not z then tryDir ds
            else do
              nvcc_exe <- findCudaTool os "nvcc"
              let cuda_include_dir = takeDirectory (takeDirectory nvcc_exe) ++ "\\include"
              z <- doesDirectoryExist cuda_include_dir
              unless z $
                fatal "can't find $CUDA include directory"
              windows_kits_dirs <- findWindowsKitsInclude
              return $ [
                  "--compiler-bindir", vs_dir
                -- TODO: need to look these up
                -- , "-IC:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\v9.0\\include"
                , "-I" ++ cuda_include_dir
                --
                -- , "-IC:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\INCLUDE"
                , "-I" ++ vs_dir ++ "\\INCLUDE"
                , "-I" ++ vs_dir ++ "\\ATL\\INCLUDE"
                --
                -- Windows Kits
                -- , "-IC:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.15063.0\\ucrt"
                -- ....
                ] ++ windows_kits_dirs
-- pick the newest child of:
--   C:\Program Files (x86)\Windows Kits\10\include
#endif


findWindowsKitsInclude :: IO [String]
findWindowsKitsInclude = do
  z <- doesDirectoryExist "C:\\Program Files (x86)\\Windows Kits\\10\\Include"
  if not z then return []
    -- e.g. ["10.0.10586.0","10.0.16299.0"]
    else do
      let isKitVersionedDir :: FilePath -> IO Bool
          isKitVersionedDir fp
            | not (all (\c -> isDigit c || c == '.') (takeFileName fp)) = return False
            | otherwise = do
              z <- doesDirectoryExist fp
              if not z then return False
                else doesDirectoryExist (fp </> "ucrt")
      --
      ds <- getSubPaths "C:\\Program Files (x86)\\Windows Kits\\10\\Include" >>= filterM isKitVersionedDir
      case sortOn (Down . takeFileName) ds of
        (newest:_) -> return
          [
            newest </> "ucrt"
          , newest </> "shared"
          , newest </> "um"
          , newest </> "winrt"
          ]



mkExe :: FilePath -> FilePath
#ifdef mingw32_HOST_OS
mkExe fp
  | ".exe" `isSuffixOf` fp = fp
  | otherwise = fp ++ ".exe"
#else
mkExe fp = fp
#endif

getSubPaths :: FilePath -> IO [FilePath]
getSubPaths dir =
  reverse .
    map (dir </>) .
      filter ((/=".") . take 1) <$> getDirectoryContents dir

runCudaTool :: Opts -> String -> [String] -> IO String
runCudaTool os tool args = do
  tool_exe <- findCudaTool os (mkExe tool)
  -- debugLn os $ show tool_exe ++ " " ++ show args
  execProcess os tool_exe args


execProcess :: Opts -> FilePath -> [String] -> IO String
execProcess os exe args = do
  debugLn os $ "% " ++ exe ++ "\n" ++ concatMap (\a -> "      "++a ++ "\n") args
  (ec,out,err) <- readProcessWithExitCode exe args ""
  debugLn os $ "% " ++ show ec
  case ec of
    ExitFailure ex_val -> do
      let lbl = "[" ++ dropExtension (takeFileName exe) ++ "] "
      fatal $ labelLines lbl $
        err ++ out ++ "\n" ++ "[exited " ++ show ex_val ++ "]"
    ExitSuccess -> return out

labelLines :: String -> String -> String
labelLines pfx = unlines . map (pfx++) . lines
