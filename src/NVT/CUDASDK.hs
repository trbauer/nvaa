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
import System.IO
import System.Process


------- finds a CUDA executable
findCudaTool :: Opts -> String -> IO FilePath
findCudaTool os exe_raw =
    tryEnvs
      [
        "CUDA_PATH"
      , "CUDA_PATH_V11_0"
      , "CUDA_PATH_V10_2"
      , "CUDA_PATH_V10_1"
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
            where vers = ["v11.0","v10.2","v10.1","v10.0","v9.1","v9.0","v8.0","v7.5"]
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

------- finds a CUDA executable
findCudaRoot :: IO FilePath
findCudaRoot =
    tryEnvs
      [
        "CUDA_PATH"
      , "CUDA_PATH_V11_0"
      , "CUDA_PATH_V10_2"
      , "CUDA_PATH_V10_1"
      , "CUDA_PATH_V10_0"
      , "CUDA_PATH_V9_0"
      , "CUDA_PATH_V9_1"
      , "CUDA_PATH_V8_0"
      , "CUDA_PATH_V7_5"
      ]
  where tryEnvs (e:es) = do
          mv <- lookupEnv e
          case mv of
            Nothing -> tryEnvs es
            Just d -> tryDir d (tryEnvs es)
#ifdef mingw32_HOST_OS
        tryEnvs [] =
          tryFixedPaths (map ("C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\"++) vers)
            where vers = ["v11.0","v10.2","v10.1","v10.0","v9.1","v9.0","v8.0","v7.5"]
#else
        -- TODO: /opt/nvidia/nvcc ?
        tryEnvs [] = tryFixedPaths ["/usr/local/cuda/bin"]
#endif
        tryFixedPaths (d:ds) = tryDir d (tryFixedPaths ds)
        tryFixedPaths [] = giveUp

        tryDir d keep_looking = do
          z <- doesFileExist (d </> "bin" </> mkExe "nvcc")
          if not z then keep_looking else return d

        giveUp :: IO FilePath
        giveUp = fatal "findCudaToolDir: failed to find CUDA SDK"
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
findExtraNvccOpts os = body
  where body :: IO [String]
        body = do
          cuda_root <- findCudaRoot
          vs_dir <- findVsDir
          --
          let cuda_include_dir = cuda_root ++ "\\include"
          z <- doesDirectoryExist cuda_include_dir
          unless z $
            fatal "can't find $CUDA include directory"
          let cuda_args =
                [
                  "--compiler-bindir", vs_dir
                --
                , "-I" ++ cuda_include_dir
                ]
          -- Windows Kits includes
          -- e.g. "-IC:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.15063.0\\ucrt"
          -- ....
          windows_kits_dirs <- findWindowsKitsInclude
          --
          return $ cuda_args ++ windows_kits_dirs


findVsDir :: IO FilePath
findVsDir = do
    mnew <- findNewVsDir
    case mnew of
      Just new -> return new
      Nothing -> do
        mold <- findOldVsDir
        case mold of
          Nothing -> fatal "cannot find an appropriate Visual Studio directory"
          Just old -> return old
  where -- VS 2017/2019
        findNewVsDir :: IO (Maybe FilePath)
        findNewVsDir =
            srch [
                -- C:\Program Files (x86)\Microsoft Visual Studio\2019\Professional\VC\Tools\MSVC
                "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Professional\\VC\\Tools\\MSVC"
                -- C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\VC\Tools\MSVC
              , "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Professional\\VC\\Tools\\MSVC"
              ]
          where srch [] = return Nothing
                srch (dir:dirs) = do
                  z <- doesDirectoryExist dir
                  if not z then srch dirs
                    else do
                      es <- listDirectory dir -- e.g. "14.23.28105"
                      let mkClPath ver = dir ++ "\\" ++ ver ++ "\\bin\\Hostx64\\x64\\cl.exe"
                      eds <- filterM (doesFileExist . mkClPath) es
                      -- pick the newest
                      case sortOn Down eds of
                        [] -> srch dirs
                        (ver:_) -> return $ Just $ takeDirectory (mkClPath ver)

        findOldVsDir :: IO (Maybe FilePath)
        findOldVsDir =
            srch [
              "C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC"
            , "C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\VC"
            ]
          where srch (dir:ds) = do
                  z <- doesDirectoryExist (dir ++ "\\BIN")
                  if not z then srch ds
                    else do
                      return $ Just dir
                srch [] = return Nothing


-- pick the newest child of:
--   C:\Program Files (x86)\Windows Kits\10\include\
findWindowsKitsInclude :: IO [String]
findWindowsKitsInclude = do
  let windows_kits_root = "C:\\Program Files (x86)\\Windows Kits\\10\\Include"
  z <- doesDirectoryExist windows_kits_root
  if not z then fatal $ windows_kits_root ++ ": cannot find Windows SDK"
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
      ds0 <- map ((windows_kits_root++"/")++) <$> listDirectory windows_kits_root
      ds <- filterM isKitVersionedDir ds0
      case sortOn (Down . takeFileName) ds of
        (newest_kit:_) -> return
          [
            "-I" ++ newest_kit </> "ucrt"
          , "-I" ++ newest_kit </> "shared"
          , "-I" ++ newest_kit </> "um"
          , "-I" ++ newest_kit </> "winrt"
          ]
#endif




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
  execProcess os tool_exe args

runCudaToolWithExitCode :: Opts -> String -> [String] -> IO (ExitCode,String,String)
runCudaToolWithExitCode os tool args = do
  tool_exe <- findCudaTool os (mkExe tool)
  execProcessWithExitCode os tool_exe args

execProcess :: Opts -> FilePath -> [String] -> IO String
execProcess os exe args = do
  (ec,out,err) <- execProcessWithExitCode os exe args
  case ec of
    ExitFailure ex_val -> do
      let lbl = "[" ++ dropExtension (takeFileName exe) ++ "] "
      fatal $ labelLines lbl $
        err ++ out ++ "\n" ++ "[exited " ++ show ex_val ++ "]"
    ExitSuccess -> do
      unless (null err) $
        hPutStrLn stderr $
          unlines (map (\ln -> "[" ++ dropExtension (takeFileName exe) ++ "] " ++ ln) (lines err))
      return out

execProcessWithExitCode :: Opts -> FilePath -> [String] -> IO (ExitCode,String,String)
execProcessWithExitCode os exe args = do
  debugLn os $ "% " ++ exe ++ "\n" ++ concatMap (\a -> "      "++a ++ "\n") args
  ret@(ec,out,err) <- readProcessWithExitCode exe args ""
  debugLn os $ "% " ++ show ec
  return ret

labelLines :: String -> String -> String
labelLines pfx = unlines . map (pfx++) . lines
