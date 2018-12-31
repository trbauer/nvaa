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

          -- let dir = "C:\\ProgramData\\NVIDIA Corporation\\CUDA Samples\\v9.0\\0_Simple"
          createDirectoryIfMissing True $ "samples/" ++ oArch os

          let dir = "C:\\ProgramData\\NVIDIA Corporation\\CUDA Samples\\v10.0"
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

          samples_inc_dir <- findCudaSamplesDir

          runWithOpts
            (os{ oOutputFile = sass_output
               , oSaveCuBin = cubin_output
               , oSavePtx = ptx_output
               , oExtraArgs = ["-I"++samples_inc_dir]
               , oInputFile = f}) `catch` handler
          copyFile f $ "samples/" ++ oArch os ++ "/" ++ takeFileName f
          walkCuFiles fs


--        /*0008*/     MOV R1, c[0x0][0x20];      /* 0x4c98078000870001 */


findCudaSamplesDir :: IO FilePath
findCudaSamplesDir = tryPathVers ["v10.0","v9.1","v9.0","v8.0"]
  where tryPathVers [] = return ""
        tryPathVers (p:ps) = do
          z <- doesDirectoryExist (mkCudaSampleDir p)
          if z then return (mkCudaSampleDir p)
            else tryPathVers ps

        mkCudaSampleDir :: String -> FilePath
        mkCudaSampleDir ver =
          "C:\\ProgramData\\NVIDIA Corporation\\CUDA Samples\\" ++ ver ++ "\\common\\inc"

