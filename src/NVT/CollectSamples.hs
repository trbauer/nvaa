module NVT.CollectSamples where

import NVT.Driver

import Control.Monad
import Control.Exception
import System.Directory
import System.FilePath
import System.Exit


collectSampleIsa :: Opts -> IO ()
collectSampleIsa os = body
  where body = do
          when (null (oArch os)) $
            die "options need arch set"
          --
          createDirectoryIfMissing True $ "samples/" ++ oArch os
          --
          -- let dir = "C:\\ProgramData\\NVIDIA Corporation\\CUDA Samples\\v10.0"
          dir <- findCudaSamplesDir
          --
          ss <- getSubPaths dir -- e.g. 0_Simple, 1_Utilities, ...
          mapM_ walkSampleSet ss

        walkSampleSet :: FilePath -> IO ()
        walkSampleSet dir = do
          ss <- getSubPaths dir -- e.g. asyncAPI
          filterM doesDirectoryExist ss >>= walkSamples

        walkSamples :: [FilePath] -> IO ()
        walkSamples [] = return ()
        walkSamples (d:ds) = do
          fs <- getSubPaths d
          walkCuFiles (filter ((==".cu") . takeExtension) fs)
          walkSamples ds

        walkCuFiles :: [FilePath] -> IO ()
        walkCuFiles [] = return ()
        walkCuFiles (f:fs) = do
          putStrLn f
          let output_no_ext = "samples/" ++ oArch os ++ "/" ++ dropExtension (takeFileName f)
          let sass_output = output_no_ext ++ ".sass"
          let ptx_output = output_no_ext ++ ".ptx"
          let cubin_output = output_no_ext ++ ".cubin"
          print cubin_output
          let handler :: SomeException -> IO ()
              handler e = do
                print e
                return ()
          --
          samples_inc_dir <- (++"\\common\\inc") <$> findCudaSamplesDir
          --
          runWithOpts
            (os{ oOutputFile = sass_output
               , oSaveCuBin = cubin_output
               , oSavePtx = ptx_output
               , oExtraArgs = ["-I"++samples_inc_dir]
               , oInputFile = f}) `catch` handler
          copyFile f $ "samples/" ++ oArch os ++ "/" ++ takeFileName f
          walkCuFiles fs


findCudaSamplesDir :: IO FilePath
findCudaSamplesDir = tryPathVers ["v10.0","v9.1","v9.0","v8.0"]
  where tryPathVers [] = return ""
        tryPathVers (p:ps) = do
          z <- doesDirectoryExist (mkCudaSampleDir p)
          if z then return (mkCudaSampleDir p)
            else tryPathVers ps

        mkCudaSampleDir :: String -> FilePath
        mkCudaSampleDir ver =
          "C:\\ProgramData\\NVIDIA Corporation\\CUDA Samples\\" ++ ver

-- "C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v10.0\bin\"

collectLibrarySampleIsa :: Opts -> IO ()
collectLibrarySampleIsa os = body
  where body = do
          when (null (oArch os)) $
            die "options need arch set"

          cuod_exe <- findCudaTool "cuobjdump"
          let dll_dir = takeDirectory cuod_exe
          dumpLibs cuod_exe dll_dir

        dumpLibs :: IO ()
        dumpLibs cuod_exe dll_dir = body
          where body = do
                  putStrLn $ "EMITTING LIBS FROM: " ++ dll_dir
                  -- dumpLib "cublas64_100.dll" -- 65 MB
                  -- dumpLib "cufft64_100.dll" -- 99 MB
                  dumpLib "curand64_100.dll" -- 48 MB
                  -- dumpLib "cusolver64_100.dll" -- 125 MB
                  -- dumpLib "cusparse64_100.dll" -- 55 MB
                  -- dumpLib "nvgraph64_100.dll" -- 68 MB

                dumpLib lib = do
                  let full_path = dll_dir ++ "/" ++ lib
                  z <- doesFileExist full_path
                  if not z then putStrLn (lib ++ ": file not found in dir: SKIPPING")
                    else do
                      out <- readProcess cuod_exe ["--list-elf",full_path] ""
                      putStr out
                      filter (("." ++ osArch os ++ ".")`isInfixOf`) (lines out)



