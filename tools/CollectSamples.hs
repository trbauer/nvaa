module Main where

import qualified NVT.CUDASDK as D
import qualified NVT.Driver as D
import qualified NVT.Opts as D

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Data.IORef
import Data.List
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.IO
import System.Process

cPP_FILT :: FilePath
cPP_FILT = "C:\\Program Files\\Haskell Platform\\8.6.3\\mingw\\bin\\c++filt.exe"

cUDA_SAMPLES_ROOT :: FilePath
cUDA_SAMPLES_ROOT = "C:\\ProgramData\\NVIDIA Corporation\\CUDA Samples"

-- "C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v10.0\bin\"
main :: IO ()
main = do
  checkSetup
  getArgs >>= run

run :: [String] -> IO ()
run as =
  case as of
    ["samples"] -> collectSampleIsa "" D.dft_opts_80
    ["libs"] -> collectLibrarySampleIsa "" D.dft_opts_80
    _ -> die "usage: collect (samples|libs)"


checkSetup :: IO ()
checkSetup = do
  let checkFile f = do
        z <- doesFileExist f
        unless z $ error $ f ++ ": file not found"
  let checkDir f = do
        z <- doesDirectoryExist f
        unless z $ error $ f ++ ": dir not found"
  checkFile cPP_FILT
  checkDir cUDA_SAMPLES_ROOT


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

        clobber :: Bool
        clobber = False

        fail_fast :: Bool
        fail_fast = False

        parallelism :: Int
        parallelism = 8

        walkSamples :: [FilePath] -> IO ()
        walkSamples [] = return ()
        walkSamples (d:ds)
          | parallelism == 1 = do
            putStrLn $ d
            walkSample putStr d `catch` serialHandler
            walkSamples ds
          | otherwise = parallelWalk
          where serialHandler :: SomeException -> IO ()
                serialHandler se = do
                  putStrLn $ "  ==> " ++ show se
                  if fail_fast then throwIO se
                    else putStrLn "continuing (fail_fast not set)"

                parallelWalk :: IO ()
                parallelWalk = do
                  case splitAt parallelism ds of
                    (ds_par,ds_sfx) -> do
                      mapM_ (\d -> putStrLn ("spawning " ++ d ++ " ...")) ds_par
                      iors <- sequence $ replicate (length ds_par) (newIORef [])
                      let prs = map (\ior -> \msg -> modifyIORef ior (msg:)) iors
                      mvs <- sequence $ replicate (length ds_par) newEmptyMVar
                      let runOne (d_par,pr,mv) = do
                            let childHandler :: SomeException -> IO ()
                                childHandler se = do
                                  pr $ "exiting on exception: " ++ show se
                                  putMVar mv (Just se)
                            forkIO $ do
                              (walkSample pr d_par >> putMVar mv Nothing) `catch` childHandler

                      mapM_ runOne (zip3 ds_par prs mvs)
                      ses <- mapM takeMVar mvs
                      forM_ (zip3 ds_par iors ses) $ \(d_par,ior,mse) -> do
                        msgs <- concat . reverse <$> readIORef ior
                        putStrLn d_par
                        putStrLn msgs
                        case mse of
                          Just (SomeException se) -> serialHandler (SomeException se)
                          Nothing -> return ()
                      walkSamples ds_sfx

        walkSample :: (String -> IO ()) -> FilePath -> IO ()
        walkSample pr d = do
          d_fs <- D.getSubPaths d
          -- TODO: should also take .cpp and .c files that #include .cuh files
          let cu_files = filter ((==".cu") . takeExtension) d_fs
          mapM_ (walkCuFile pr) (filter (filter_str`isInfixOf`) cu_files)
          --
          -- copy any .cuh files too from this sample too
          let cuh_files = filter (".cuh"`isSuffixOf`) d_fs
          forM_ cuh_files $ \cuh_file -> do
            pr $ "  copying " ++ cuh_file ++ "\n"
            copyFile cuh_file (base_output_dir ++ "/" ++ takeFileName cuh_file)

        walkCuFile ::  (String -> IO ()) ->FilePath -> IO ()
        walkCuFile pr src_cu_file = do
            all_exist <- and <$> mapM doesFileExist [sass_output, ptx_output, cubin_output]
            if all_exist && not clobber then skipIt
              else buildIt
          where output_no_ext = base_output_dir ++ "/" ++ dropExtension (takeFileName src_cu_file)
                sass_output = output_no_ext ++ ".sass"
                ptx_output = output_no_ext ++ ".ptx"
                cubin_output = output_no_ext ++ ".cubin"

                skipIt = do
                  pr "   ==> already built\n"

                buildIt = do
                  pr "   ==> building\n"
                  samples_inc_dir <- (++"\\common\\inc") <$> findCudaSamplesDir
                  --
                  let nvcc_os =
                        os {
                          D.oOutputFile = sass_output
                        , D.oSaveCuBin = cubin_output
                        , D.oSavePtx = ptx_output
                        , D.oExtraArgs = ["-I"++samples_inc_dir]
                        , D.oInputFile = src_cu_file
                        }
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
                          pr "  ====> re-trying with -rdc=true\n"
                          D.runWithOpts
                            nvcc_os{D.oRDC = True} `catch` handler True
                        | otherwise =
                          throwIO e -- propagate up
                  --
                  D.runWithOpts nvcc_os `catch` handler False
                  -- copies the .cu file
                  let dst_cu_file = base_output_dir ++ "/" ++ takeFileName src_cu_file
                  copyFile src_cu_file dst_cu_file
                  pr $ "  ==> done; copied " ++ dst_cu_file

findCudaSamplesDir :: IO FilePath
findCudaSamplesDir = tryPathVers ["v11.0","v10.2","v10.1","v10.0","v9.1","v9.0","v8.0"]
  where tryPathVers [] = return ""
        tryPathVers (p:ps) = do
          z <- doesDirectoryExist (mkCudaSampleDir p)
          if z then return (mkCudaSampleDir p)
            else tryPathVers ps

        mkCudaSampleDir :: String -> FilePath
        mkCudaSampleDir ver = cUDA_SAMPLES_ROOT ++ "\\" ++ ver


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

                  tryLib "nppial64"     --   9 MB
                  tryLib "nppicc64"     --   3 MB
                  tryLib "nppicom64"    --   3 MB
                  tryLib "nppidei64"    --
                  tryLib "nppif64"      --
                  tryLib "nppig64"      --
                  tryLib "nppim64"      --
                  tryLib "nppist64"     --
                  tryLib "nppisu64"     -- no device code
                  tryLib "nppitc64"     --
                  tryLib "npps64"       --
                  tryLib "nvjpeg64"     --   3 MB

                  tryLib "curand64"     --  48 MB
                  tryLib "cusparse64"   --  55 MB
                  tryLib "cublas64"     --  65 MB
                  tryLib "nvgraph64"    --  68 MB
                  tryLib "cufft64"      --  99 MB
                  tryLib "cusolver64"   -- 125 MB

                tryLib :: String -> IO ()
                tryLib lib = tryExts ["_11.dll","_10.dll"]
                  where tryExts [] = return ()
                        tryExts (e:es) = do
                          z <- doesFileExist (dll_dir ++ "/" ++ lib ++ e)
                          if z then dumpLib (lib ++ e)
                            else tryExts es

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
          when z $
            putStrLn "... skipping (already dumped)"
          unless z $ do
            putStrLn $ "  % cuobjdump --extract-elf  " ++ elf_cubin ++ "  " ++ full_dll
            readProcess cuod_exe ["--extract-elf", elf_cubin, full_dll] ""
            --
            -- disasm to file (will be huge)
            let tmp_sass = takeFileName elf_cubin ++ "-tmp.sass"
            putStrLn $ "  % nvdisasm ... extract raw sass"
            withFile tmp_sass WriteMode $ \h_out -> do
              D.runCudaToolToHandle D.dft_opts{D.oArch = D.oArch os} "nvdisasm" [
                        "--no-vliw"
                      , "--no-dataflow"
                      , "--print-line-info"
                      , "--print-instruction-encoding"
                      , elf_cubin
                      ] h_out
             --
            putStrLn $ "  % nva ... filtering assembly output"
            D.runWithOpts
              os {
                D.oInputFile = tmp_sass
              , D.oOutputFile = output_dir ++ "/" ++ dropExtension elf_cubin ++ ".sass"
              }
            removeFile tmp_sass

            --  appendFile (output_prefix ++ ".sass") oup
            -- D.runWithOpts
            --   os {
            --     D.oInputFile = elf_cubin
            --   , D.oOutputFile = output_dir ++ "/" ++ dropExtension elf_cubin ++ ".sass"
            --   , D.oTextOnly = True
            --   -- , D.oExtraArgs = D.oExtraArgs os ++ ["--dump-resource-usage"]
            --   }

            res <- readProcess cuod_exe ["--dump-resource-usage", elf_cubin] ""
            res_cpp <- readProcess cPP_FILT [] res
            writeFile (replaceExtension dst_elf_cubin "txt") res_cpp
            renameFile elf_cubin dst_elf_cubin
            return ()
