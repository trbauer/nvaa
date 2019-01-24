module Main where

import qualified NVT.CUDASDK as D
import qualified NVT.Driver as D
import qualified NVT.Opts as D

import Control.Monad
import Control.Exception
import Data.List
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import System.Process


-- "C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v10.0\bin\"
main :: IO ()
main = do
  collectSampleIsa        "" D.dft_opts_75
--  collectLibrarySampleIsa "" D.dft_opts_75


collectSampleIsa :: String -> D.Opts -> IO ()
collectSampleIsa filter_str os_raw = body
  where body = do
          when (null (D.oArch os)) $
            die "options need arch set"
          --
          createDirectoryIfMissing True base_output_dir
          --
          -- let dir = "C:\\ProgramData\\NVIDIA Corporation\\CUDA Samples\\v10.0"
          dir <- findCudaSamplesDir
          --
          ss <- D.getSubPaths dir >>= filterM doesDirectoryExist -- e.g. 0_Simple, 1_Utilities, ...
          mapM_ walkSampleSet ss

        os = os_raw{D.oSourceMapping = True}

        base_output_dir :: FilePath
        base_output_dir = "examples/" ++ D.oArch os ++ "/samples"

        walkSampleSet :: FilePath -> IO ()
        walkSampleSet dir = do
          let filterNonNvrtc :: [FilePath] -> [FilePath]
              filterNonNvrtc =
                -- filter (not . ("asyncAPI"`isInfixOf`)) .
                  filter (not . ("_nvrtc"`isInfixOf`))
          ss <- filterNonNvrtc <$> D.getSubPaths dir -- e.g. asyncAPI
          filterM doesDirectoryExist ss >>= walkSamples

        walkSamples :: [FilePath] -> IO ()
        walkSamples [] = return ()
        walkSamples (d:ds) = do
          fs <- D.getSubPaths d
          -- TODO: should also take .cpp and .c files that #include .cuh files
          let cu_files = filter ((==".cu") . takeExtension) fs
          walkCuFiles (filter (filter_str`isInfixOf`) cu_files)
          walkSamples ds

        walkCuFiles :: [FilePath] -> IO ()
        walkCuFiles [] = return ()
        walkCuFiles (src_cu_file:fs) = do
          putStrLn src_cu_file
          let output_no_ext = base_output_dir ++ "/" ++ dropExtension (takeFileName src_cu_file)
          let sass_output = output_no_ext ++ ".sass"
          let ptx_output = output_no_ext ++ ".ptx"
          let cubin_output = output_no_ext ++ ".cubin"
          --
          samples_inc_dir <- (++"\\common\\inc") <$> findCudaSamplesDir
          --
          let nvcc_os = os
               { D.oOutputFile = sass_output
               , D.oSaveCuBin = cubin_output
               , D.oSavePtx = ptx_output
               , D.oExtraArgs = ["-I"++samples_inc_dir]
               , D.oInputFile = src_cu_file}
          let handler :: Bool -> SomeException -> IO ()
              handler double_fault e
                | "user interrupt" `isInfixOf` show e = do
                  putStrLn $  "=== interrupt ==="
                  throwIO e
                --
                -- | "requires separate compilation mode" `isInfixOf` show e && not (D.oRDC os) = do
                | "ExitFailure" `isInfixOf` show e && not double_fault = do
                -- D.runWithOpts will exit non-zero, the separate compilation mode
                -- output goes to our stderr and it exits (which is an exception).
                  putStrLn "  ====> re-trying with -rdc=true"
                  D.runWithOpts
                    nvcc_os{D.oRDC = True} `catch` handler True
                | otherwise = do
                  putStrLn "=== caught other exception ==="
                  print e
                  -- throwIO e
                  return ()
          D.runWithOpts
            nvcc_os `catch` handler False
          -- copies the .cu file
          let dst_cu_file = base_output_dir ++ "/" ++ takeFileName src_cu_file
          copyFile src_cu_file dst_cu_file
          -- TODO: copy and .cuh files
          -- TODO: map source lines

          -- for the last .cu file, copy any .cuh files too
          when (null fs) $ do
            cuh_files <- filter ((".cuh"`isSuffixOf`)) <$> D.getSubPaths (takeDirectory src_cu_file)
            forM_ cuh_files $ \cuh_file -> do
              copyFile cuh_file (replaceFileName dst_cu_file (takeFileName cuh_file))
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


collectLibrarySampleIsa :: String -> D.Opts -> IO ()
collectLibrarySampleIsa substr os_raw = body
  where body = do
          when (null (D.oArch os)) $
            die "options need arch set"
          --
          cuod_exe <- D.findCudaTool os "cuobjdump"
          nvdis_exe <- D.findCudaTool os "nvdisasm"
          let dll_dir = takeDirectory cuod_exe
          dumpLibs cuod_exe nvdis_exe dll_dir

        os = os_raw{D.oSourceMapping = True}

        dumpLibs :: FilePath -> FilePath -> FilePath -> IO ()
        dumpLibs cuod_exe nvdis_exe dll_dir = body
          where body = do
                  putStrLn $ "EMITTING LIBS FROM: " ++ dll_dir

                  dumpLib "nppial64_100.dll"     --   9 MB
                  dumpLib "nppicc64_100.dll"     --   3 MB
                  dumpLib "nppicom64_100.dll"    --   3 MB
                  dumpLib "nppidei64_100.dll"    --
                  dumpLib "nppif64_100.dll"      --
                  dumpLib "nppig64_100.dll"      --
                  dumpLib "nppim64_100.dll"      --
                  dumpLib "nppist64_100.dll"     --
                  dumpLib "nppitc64_100.dll"     --
                  dumpLib "npps64_100.dll"       --

                  dumpLib "curand64_100.dll"     --  48 MB
                  dumpLib "cusparse64_100.dll"   --  55 MB
                  dumpLib "cublas64_100.dll"     --  65 MB
                  dumpLib "nvgraph64_100.dll"    --  68 MB
                  dumpLib "cufft64_100.dll"      --  99 MB
                  dumpLib "cusolver64_100.dll"   -- 125 MB

                dumpLib :: String -> IO ()
                dumpLib lib
                  | not (substr `isInfixOf` lib) = return ()
                  | otherwise = do
                    putStrLn $ "*** DUMPING " ++ lib
                    let full_lib_path = dll_dir ++ "/" ++ lib
                    z <- doesFileExist full_lib_path
                    if not z then putStrLn (lib ++ ": file not found in dir: SKIPPING")
                      else collectLibrarySampleIsaFromDir os (full_lib_path)

-- should generalize the top one to call this one
collectLibrarySampleIsaFromDir :: D.Opts -> FilePath -> IO ()
collectLibrarySampleIsaFromDir os_raw full_dll = body
  where os = os_raw{D.oSourceMapping = True}

        body = do
          putStrLn $ "*** DUMPING " ++ full_dll
          cuod_exe <- D.findCudaTool os "cuobjdump"
          out <- readProcess cuod_exe ["--list-elf",full_dll] ""
          let elfs_lines = filter (("." ++ D.oArch os ++ ".")`isInfixOf`) (lines out)
          let output_dir = "examples/" ++ D.oArch os ++ "/libs/" ++ takeFileName (dropExtension full_dll)
          createDirectoryIfMissing True output_dir
          mapM_ (processElf cuod_exe output_dir) (map (last . words) elfs_lines)
          putStrLn "  === DUMPING PTX"
          let ptx_file = output_dir ++ "/" ++ takeFileName (dropExtension full_dll) ++ ".ptx"
          z <- doesFileExist ptx_file
          unless z $ do
            -- burns up too much memory
            -- oup <- readProcess cuod_exe ["--dump-ptx", full_lib_path] ""
            -- writeFile (output_prefix ++ ".ptx") oup
            withFile ptx_file WriteMode $ \h -> do
              hSetNewlineMode h nativeNewlineMode{outputNL = LF}
              let cp =
                    (proc cuod_exe ["--dump-ptx",full_dll]) {
                      std_out = UseHandle h
                    }
              (Nothing,Nothing,Nothing,ph) <- createProcess cp
              ec <- waitForProcess ph
              putStrLn $ "  ptx dumping => " ++ show ec

        processElf :: FilePath -> FilePath -> String -> IO ()
        processElf cuod_exe output_dir elf_cubin = do
          putStrLn $ "  dumping " ++ elf_cubin
          let dst_elf_cubin = output_dir ++ "/" ++ elf_cubin
          z <- doesFileExist dst_elf_cubin
          unless z $ do
            readProcess cuod_exe ["--extract-elf", elf_cubin, full_dll] ""
            -- oup <- readProcess nvdis_exe [
            --           "--no-vliw"
            --         , "--no-dataflow"
            --         , "--print-line-info"
            --         , "--print-instruction-encoding"
            --         , elf
            --         ] ""
            --  appendFile (output_prefix ++ ".sass") oup
            D.runWithOpts
              os {
                D.oInputFile = elf_cubin
              , D.oOutputFile = output_dir ++ "/" ++ dropExtension elf_cubin ++ ".sass"
              , D.oTextOnly = True
              -- , D.oExtraArgs = D.oExtraArgs os ++ ["--dump-resource-usage"]
              }
            res <- readProcess cuod_exe ["--dump-resource-usage", elf_cubin] ""
            res_cpp <- readProcess ("C:\\Haskell\\8.4.2\\mingw\\bin\\c++filt.exe") [] res
            writeFile (replaceExtension dst_elf_cubin "txt") res_cpp
            renameFile elf_cubin dst_elf_cubin
            return ()
