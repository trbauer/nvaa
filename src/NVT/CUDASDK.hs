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

sdks :: [(Int,Int)]
sdks =
  reverse [
    (7,5)
  , (8,0)
  , (9,1)
  , (9,1)
  , (9,1)
  , (10,0)
  , (10,1)
  , (10,2)
  , (11,0)
  , (11,1)
  , (11,2)
  , (11,3)
  , (11,4)
  , (11,5)
  , (11,6)
  , (12,0)
  -- disable until I can get OpenCL C working again
  -- , (12,3)
  --   guessing
  ]

cuda_env_vars :: [String]
cuda_env_vars = "CUDA_PATH":["CUDA_PATH_V" ++ show mj ++ "_" ++ show mi | (mj,mi)<-sdks]

-- e.g. v11.0 (under cuda toolkit dir)
cuda_ver_exts :: [String]
cuda_ver_exts = ["v" ++ show mj ++ "." ++ show mi | (mj,mi)<-sdks]

------- finds a CUDA executable
findCudaTool :: Opts -> String -> IO FilePath
findCudaTool os exe_raw = tryEnvs cuda_env_vars
  where exe = mkExe exe_raw

        tryEnvs (e:es) = do
          mv <- lookupEnv e
          case mv of
            Nothing -> tryEnvs es
            Just d -> do
              tryPath (d </> "bin" </> exe) (tryEnvs es)
#ifdef mingw32_HOST_OS
        tryEnvs [] =
          tryFixedPaths (map ("C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\"++) cuda_ver_exts)
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
findCudaRoot = tryEnvs cuda_env_vars

  where tryEnvs (e:es) = do
          mv <- lookupEnv e
          case mv of
            Nothing -> tryEnvs es
            Just d -> tryDir d (tryEnvs es)
#ifdef mingw32_HOST_OS
        tryEnvs [] =
          tryFixedPaths (map ("C:\\Program Files\\NVIDIA GPU Computing Toolkit\\CUDA\\"++) cuda_ver_exts)
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
  where -- VS 2017/2019/2022
        findNewVsDir :: IO (Maybe FilePath)
        findNewVsDir =
            srch [
                "C:\\Program Files\\Microsoft Visual Studio\\2022\\Enterprise\\VC\\Tools\\MSVC"
              , "C:\\Program Files\\Microsoft Visual Studio\\2022\\Professional\\VC\\Tools\\MSVC"
              , "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC"
              , "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Enterprise\\VC\\Tools\\MSVC"
              , "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Professional\\VC\\Tools\\MSVC"
              , "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC"
              , "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Enterprise\\VC\\Tools\\MSVC"
              , "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Professional\\VC\\Tools\\MSVC"
              , "C:\\Program Files (x86)\\Microsoft Visual Studio\\2017\\Community\\VC\\Tools\\MSVC"
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




#ifdef mingw32_HOST_OS
mkExe :: FilePath -> FilePath
mkExe fp
  | ".exe" `isSuffixOf` fp = fp
  | otherwise = fp ++ ".exe"
shellLineSplice :: String
shellLineSplice = "^"
#else
mkExe :: FilePath -> FilePath
mkExe = id

shellLineSplice :: String
shellLineSplice = "\\"
#endif


runCudaTool :: Opts -> String -> [String] -> IO String
runCudaTool os tool args = do
  (ec,out,err) <- runCudaToolWithExitCode os tool args
  case ec of
    ExitFailure ex_val -> do
      fatal $ labelLines ("[" ++ mkExe tool ++ "] ") $
        err ++ out ++ "\n" ++ "[exited " ++ show ex_val ++ "]"
    ExitSuccess -> do
      unless (null err) $
        hPutStrLn stderr $
          unlines (map (\ln -> "[" ++ mkExe tool ++ "] " ++ ln) (lines err))
      return out

fmtCommandLine :: String -> [String] -> String
fmtCommandLine exe as =
    "% " ++ intercalate (shellLineSplice ++ "\n" ++ "      ") (map escArg (exe:as))
  where escArg a
          | any isSpace a = "\"" ++ a ++ "\""
          | otherwise = a

runCudaToolWithExitCode :: Opts -> String -> [String] -> IO (ExitCode,String,String)
runCudaToolWithExitCode os tool args = do
  tool_exe <- findCudaTool os (mkExe tool)
  debugLn os $ fmtCommandLine (mkExe tool) args
  r@(ec,_,_) <- readProcessWithExitCode tool_exe args ""
  debugLn os $ "  ==> " ++ show ec
  return r


runCudaToolToHandle :: Opts -> String -> [String] -> Handle -> IO ()
runCudaToolToHandle os tool args h_out = do
  tool_exe <- findCudaTool os (mkExe tool)
  debugLn os $ fmtCommandLine (mkExe tool) args
  ph <- runProcess tool_exe args Nothing Nothing Nothing (Just h_out) (Just stderr)
  ec <- waitForProcess ph
  debugLn os $ "  ==> " ++ show ec
  case ec of
    ExitFailure ex_val -> do
      fatal $ labelLines ("[" ++ tool_exe ++ "] ") $ "[exited " ++ show ex_val ++ "]"
    ExitSuccess -> return ()


labelLines :: String -> String -> String
labelLines pfx = unlines . map (pfx++) . lines


getSubPaths :: FilePath -> IO [FilePath]
getSubPaths dir =
  reverse .
    map (dir </>) .
      filter ((/=".") . take 1) <$> getDirectoryContents dir

