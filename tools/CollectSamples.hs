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
import Debug.Trace
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.IO
import System.Process

cPP_FILT :: FilePath
-- cPP_FILT = "C:\\Program Files\\Haskell Platform\\8.6.3\\mingw\\bin\\c++filt.exe"
-- cPP_FILT = "C:\\Program Files\\Haskell Platform\\8.6.5\\mingw\\bin\\c++filt.exe"
-- cPP_FILT = C:\ProgramData\chocolatey\lib\ghc\tools\ghc-8.10.2\mingw\bin
cPP_FILT = "c++filt.exe"
-- "cu++filt"

cUDA_SAMPLES_ROOT :: FilePath
-- cUDA_SAMPLES_ROOT = "C:\\ProgramData\\NVIDIA Corporation\\CUDA Samples"
cUDA_SAMPLES_ROOT = "./cuda-samples" -- git clone https://github.com/nvidia/cuda-samples

vULKAN_SDK :: FilePath
-- vULKAN_SDK = "C:\\vulkanSDK\\1.2.189.2\\Include"
vULKAN_SDK = "C:\\vulkanSDK\\1.3.236.0\\Include"

dft_opts :: D.Opts
-- dft_opts = D.dft_opts_86
dft_opts = D.dft_opts_90 -- sm_90

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run as = do
  checkSetup
  parseCSOpts cso_dft as >>= runWithOpts

runWithOpts :: CSOpts -> IO ()
runWithOpts cso = do
  case csoTool cso of
    "samples" -> collectSampleIsa cso dft_opts
    "libs" -> collectLibrarySampleIsa cso dft_opts
    t -> fatal ("-t=" ++ t ++ ": invalid tool")


fatal :: String -> IO a
fatal = die

data CSOpts =
  CSOpts {
    csoTool :: String
  , csoFilters :: ![String]
  , csoParallelism :: !Int
  , csoClobber :: !Bool
  , csoFailFast :: !Bool
  , csoVerbosity :: !Int
  } deriving Show
cso_dft :: CSOpts
cso_dft =
  CSOpts {
    csoTool = ""
  , csoFilters = []
  , csoParallelism = 1
  , csoClobber = False
  , csoFailFast = False
  , csoVerbosity = 0
  }
csoMatchesFilter :: CSOpts -> String -> Bool
csoMatchesFilter cso str =
  null (csoFilters cso) || any (`isInfixOf`str) (csoFilters cso)


parseCSOpts :: CSOpts -> [String] -> IO CSOpts
parseCSOpts cso [] = return cso
parseCSOpts cso (a:as)
  | a `elem` ["-h","--help"] = do
    putStrLn $
      "usage:  collect  OPTS -t=(samples|libs) FILTERS\n" ++
      "  where OPTS are:\n" ++
      "     -c            clobber\n" ++
      "     -f            fail fast\n" ++
      "     -j[=INT]      parallelism\n" ++
      "     -t=TOOL       sets the tool (must be samples or libs)\n" ++
      "     -v[=INT]      verbosity\n" ++
      "  and FILTERS are a list of infix matches to include\n" ++
      ""
    exitSuccess
  | a `elem` ["-t"] = badArg ("expected " ++ a ++ "=...")
  | a == "-c" = parseCSOpts (cso{csoClobber = True}) as
  | a`elem`["-f","--fail-fast"]  = parseCSOpts (cso{csoFailFast = True}) as
  | a == "-j" = parseCSOpts (cso{csoParallelism = 8}) as
  | "-j="`isPrefixOf`a = handleIntArg (\x -> cso{csoParallelism = x})
  | a == "-v" = parseCSOpts (cso{csoVerbosity = 1}) as
  | "-t="`isPrefixOf`a =
      if null (csoTool cso)
        then parseCSOpts (cso{csoTool = eq_val}) as
        else badArg "tool already set"
  | "-v="`isPrefixOf`a = handleIntArg (\x -> cso{csoVerbosity = x})
  | "-"`isPrefixOf`a = badArg "unrecognized option"
  | otherwise = parseCSOpts (cso{csoFilters = csoFilters cso ++ [a]}) as
  where badArg msg = fatal $ a ++ ": " ++ msg
        eq_val = drop 1 . dropWhile (/='=') $ a
        handleIntArg func =
          case reads eq_val of
            [(x,"")] -> parseCSOpts (func x) as


checkSetup :: IO ()
checkSetup = do
  let checkFile f = do
        z <- doesFileExist f
        unless z $ error $ f ++ ": file not found"
  let checkDir f = do
        z <- doesDirectoryExist f
        unless z $ error $ f ++ ": dir not found"
  me <- findExecutable cPP_FILT
  when (me == Nothing) $ error $ cPP_FILT ++ ": cannot find executable in $PATH"
  checkDir cUDA_SAMPLES_ROOT
  checkDir vULKAN_SDK

collectSampleIsa :: CSOpts -> D.Opts -> IO ()
collectSampleIsa cso os_raw = body
  where body = do
          print cso
          when (null (D.oArch os)) $
            die "options need arch set"
          --
          createDirectoryIfMissing True base_output_dir
          --
          -- e.g. let dir = "C:\\ProgramData\\NVIDIA Corporation\\CUDA Samples\\v10.0"
          -- under this dir we have {bin, common, 0_Simple, ...}
          -- under newer models (github samples) we have {bin, Common, Samples}
          --   where Samples has all the {0_Introduction, 1_Utilities, ..}
          -- bin, common,
          root_dir <- findCudaSamplesDir
          comp_ds <- getAllSampleDirs cso root_dir
          putStrLn "==================== RUNNING ===================="
          mapM_ (\d -> putStrLn (" => " ++ d)) comp_ds
          walkSamples comp_ds
          return ()

        os :: D.Opts
        os = os_raw {D.oPrintLines = True, D.oPrintEncoding = True, D.oPrintOffsets = True}

        base_output_dir :: FilePath
        base_output_dir = "examples/" ++ D.oArch os ++ "/samples"

        clobber :: Bool
        clobber = csoClobber cso

        fail_fast :: Bool
        fail_fast = csoFailFast cso

        parallelism :: Int
        parallelism = csoParallelism cso

        walkSamples :: [FilePath] -> IO ()
        walkSamples [] = return ()
        walkSamples (d:ds)
          | parallelism == 1 = do
            walkSample putStr d `catch` serialHandler
            walkSamples ds
          | otherwise = parallelWalk
          where serialHandler :: SomeException -> IO ()
                serialHandler se = do
                  putStrLn $ "  ==> [main] " ++ show se
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
                                  pr $ "  [" ++ takeFileName d_par ++ "] exiting on exception: " ++ show se ++ "\n"
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
          mapM_ (walkCuFile pr d) cu_files
          --
          -- copy any .cuh files too from this sample too
          let aux_files = filter (\fp -> any (`isSuffixOf`fp) [".cuh",".h",".hpp",".hxx",".cpp"]) d_fs
          forM_ aux_files $ \aux_file -> do
            pr $ "  [" ++ takeFileName d ++ "]: copying " ++ aux_file ++ "\n"
            unless (null cu_files) $ do
              let output_dir = base_output_dir ++ "/" ++ getSamplePathOutputDir (head cu_files)
              copyFile aux_file (output_dir ++ "/" ++ takeFileName aux_file)

        walkCuFile ::  (String -> IO ()) -> FilePath -> FilePath -> IO ()
        walkCuFile pr d src_cu_file = do
            -- src_cu_file = ./cuda-samples/Samples\5_Domain_Specific\vulkanImageCUDA\vulkanImageCUDA.cu
            createDirectoryIfMissing True output_dir
            all_exist <- and <$> mapM doesFileExist [sass_output, ptx_output, cubin_output]
            if all_exist && not clobber then skipIt
              else buildIt (all_exist && clobber)
          where output_dir = base_output_dir ++ "/" ++ getSamplePathOutputDir src_cu_file
                output_no_ext = output_dir ++ dropExtension (takeFileName src_cu_file)
                sass_output = output_no_ext ++ ".sass"
                ptx_output = output_no_ext ++ ".ptx"
                cubin_output = output_no_ext ++ ".cubin"

                printLn :: String -> IO ()
                printLn msg = pr $ "  [" ++ takeFileName d ++ "]: " ++ msg ++ "\n"

                skipIt = do
                  printLn "already built"

                buildIt :: Bool -> IO ()
                buildIt clobbering = do
                  printLn $ "building" ++ (if clobbering then " (clobbering)" else "")
                  --
                  -- samples_inc_dir <- (++"\\common\\inc") <$> findCudaSamplesDir
                  -- github samples
                  samples_inc_dir <- (++"\\Common") <$> findCudaSamplesDir
                  --
                  flns <- lines <$> readFile src_cu_file
                  let has_glfw = any ("<GLFW/glfw3.h>"`isInfixOf`) flns
                      maybe_glfw_opts
                        | not has_glfw = []
                        | otherwise = ["-Ideps/glfw/include"]
                  let has_vulk = any ("<vulkan/vulkan.h>"`isInfixOf`) flns
                      maybe_vulk_opts
                        | not has_vulk = []
                        | otherwise = ["-I" ++ vULKAN_SDK]
                  --
                  let nvcc_os =
                        os {
                          D.oOutputFile = sass_output
                        , D.oSaveCuBin = cubin_output
                        , D.oSavePtxTo = ptx_output
                        , D.oSavePtx = True
                        , D.oExtraNvccArgs = ["-I"++samples_inc_dir, "-I" ++ takeDirectory src_cu_file] ++
                            maybe_glfw_opts ++ maybe_vulk_opts
                        , D.oInputFile = src_cu_file
                        }
                  printLn $ "\n     % nva.exe[embedded]  " ++
                    intercalate "^\n      " ([
                      "--arch=" ++ D.oArch nvcc_os
                    , "-o=" ++ sass_output
                    , "--save-cubin=" ++ cubin_output
                    , "--save-ptx=" ++ ptx_output
                    ] ++ map ("-Xnvcc="++) (D.oExtraNvccArgs nvcc_os) ++ [src_cu_file])
                  --
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
                  let dst_cu_file = output_dir ++ takeFileName src_cu_file
                  copyFile src_cu_file dst_cu_file
                  --
                  cuod_exe <- D.findCudaTool os "cuobjdump"
                  res <- readProcess cuod_exe ["--dump-resource-usage", cubin_output] ""
                  res_cpp <- readProcess cPP_FILT [] res
                  writeFile (replaceExtension cubin_output "txt") res_cpp
                  --
                  printLn $ "  done"

-- ./cuda-samples/Samples/5_Domain_Specific/vulkanImageCUDA/vulkanImageCUDA.cu
-- want                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- sample set and sample name
getSamplePathOutputDir :: FilePath -> FilePath
getSamplePathOutputDir fp =
  case reverse (splitPath fp) of
    _:smp_dir:smp_set_dir:_ -> concat [smp_set_dir,smp_dir]

getAllSampleDirs :: CSOpts -> FilePath -> IO [FilePath]
getAllSampleDirs cso root_dir = do
    z <- doesDirectoryExist (root_dir ++ "/.git")
    all_ds <- if z then githubSamples else oldSdkSamples -- old style installed samples
    let comp_ds = filter (csoMatchesFilter cso) all_ds
    return comp_ds
  where oldSdkSamples = do
          ss <- D.getSubPaths root_dir >>= filterM doesDirectoryExist -- e.g. 0_Simple, 1_Utilities, ...
          concat <$> mapM getSampleSetSampleDirs ss

        githubSamples :: IO [FilePath]
        githubSamples = do
          ss <- D.getSubPaths (root_dir ++ "/Samples")
          concat <$> mapM getSampleSetSampleDirs ss


-- e.g. fetch all under: 2_Graphics/...
getSampleSetSampleDirs :: FilePath -> IO [FilePath]
getSampleSetSampleDirs root_dir = do
    z <- doesDirectoryExist (root_dir ++ "/.git")
    if z then githubSamples else oldSdkSamples -- old style installed samples
  where oldSdkSamples = do
          let filterNonNvrtc :: [FilePath] -> [FilePath]
              filterNonNvrtc =
                -- filter (not . ("asyncAPI"`isInfixOf`)) .
                filter (not . ("_nvrtc"`isInfixOf`))
          ss <- filterNonNvrtc <$> D.getSubPaths root_dir -- e.g. asyncAPI
          ds <- filterM doesDirectoryExist ss
          return ds

        githubSamples :: IO [FilePath]
        githubSamples = do
          ss <- D.getSubPaths root_dir
          ds <- filterM doesDirectoryExist ss
          return ds

findCudaSamplesDir :: IO FilePath
findCudaSamplesDir = do
    z <- doesDirectoryExist (cUDA_SAMPLES_ROOT ++ "/.git")
    if z then return cUDA_SAMPLES_ROOT -- github samples
      else tryPathVers -- old style installed samples
             [
               "v11.6", "v11.5", "v11.4", "v11.0"
             , "v10.2", "v10.1", "v10.0"
             , "v9.1", "v9.0"
             , "v8.0"
             ]
  where tryPathVers [] = return ""
        tryPathVers (p:ps) = do
          z <- doesDirectoryExist (mkCudaSampleDir p)
          if z then return (mkCudaSampleDir p)
            else tryPathVers ps

        mkCudaSampleDir :: String -> FilePath
        mkCudaSampleDir ver = cUDA_SAMPLES_ROOT ++ "\\" ++ ver

--------------------------------------------------------------------------------
--

collectLibrarySampleIsa :: CSOpts -> D.Opts -> IO ()
collectLibrarySampleIsa cso os_raw = body
  where body = do
          when (null (D.oArch os)) $
            die "options need arch set"
          --
          cuod_exe <- D.findCudaTool os "cuobjdump"
          nvdis_exe <- D.findCudaTool os "nvdisasm"
          let dll_dir = takeDirectory cuod_exe
          dumpLibs cuod_exe nvdis_exe dll_dir

        os =
          os_raw {
            D.oPrintLines = True
          , D.oPrintOffsets = True
          , D.oPrintEncoding = True
          }

        dumpLibs :: FilePath -> FilePath -> FilePath -> IO ()
        dumpLibs cuod_exe nvdis_exe dll_dir = body
          where body = do
                  putStrLn $ "EMITTING LIBS FROM: " ++ dll_dir

                  {-
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
                  -}
                  let libs12 :: [String]
                      libs12 =
                        -- https://docs.nvidia.com/cuda/npp/index.html
                        [                 -- SIZE (MB)
                          "cublasLt64"   --  455
                        , "cublas64"     --   95
                     --   , "cudart64"     --    0.5 NO DEV CODE
                        , "cusparse64"   --  202
                     -- , "nppc64"       --    0.282  (core)
                        , "nppial64"     --   14        (arithmetic and logical)
                        , "nppicc64"     --    5.2      (color conversion and sampling)
                        , "nppidei64"    --    9.2      (data exchange and init)
                        , "nppif64"      --   92        (filtering and computer vision)
                        , "nppig64"      --   37        (geom transform)
                        , "nppim64"      --    7        (morphological ops)
                        , "nppist64"     --   36        (stats and linear transform)
                        , "nppisu64"     --    0.2      (memory support)
                        , "nppitc64"     --    3        (threshold and compare funcs)
                        , "npps64"       --   17
                      --  , "nvblas64"     --    0.3  NO DEV CODE
                        , "nvjpeg64"     --    3 -- https://developer.nvidia.com/nvjpeg
                        ]
                  mapM_ (tryLib ["_12.dll"]) libs12


                tryLib :: [String] -> String -> IO ()
                tryLib sfxs lib = tryExts sfxs -- [...,"_11.dll","_10.dll"]
                  where tryExts [] = do
                          putStrLn $ lib ++ ": could not find lib with valid suffix"
                          return ()
                        tryExts (e:es) = do
                          let fp = dll_dir ++ "\\" ++ lib ++ e
                          z <- doesFileExist fp
                          if z then dumpLib fp
                            else tryExts es

                dumpLib :: String -> IO ()
                dumpLib lib
                  | csoMatchesFilter cso lib = collectLibrarySampleIsaFromDir os lib
                  | otherwise = putStrLn $ "============= FILTERED OUT " ++ lib



makeLibFunctionLists :: D.Opts -> IO ()
makeLibFunctionLists os = do
  let processLib :: FilePath -> IO ()
      processLib lib_dir = do
        sass_fps <- filter (".sass"`isSuffixOf`) <$> D.getSubPaths lib_dir
        let processSass :: FilePath -> IO String
            processSass sass_fp = do
              let filtGbl :: String -> [String]
                  filtGbl ln =
                    case words (map (\c -> if c == ',' then ' ' else c) ln) of
                      -- some don't have .global symbols
                      -- [".global",symbol] -> [symbol]
                      -- .type           _ZN3cub11EmptyKernelIvEEvv,@function
                      [".type",func,"@function"] -> [func]
                      _ -> []
              oup_mngld <- concatMap filtGbl . lines <$> readFile sass_fp
              oup_unmngld <- readProcess cPP_FILT [] (unlines oup_mngld)
              when (length (lines oup_unmngld) /= length oup_mngld) $
                error "mismatch in length"
              let fmtSym (m,u) = ".global " ++ m ++ "\n" ++ "  " ++ u ++ "\n"
              let oup =
                    sass_fp ++ ":\n" ++
                      concatMap fmtSym (zip oup_mngld (lines oup_unmngld))
              putStrLn oup
              return oup
        oup_glbs <- intercalate "\n" <$> mapM processSass sass_fps
        let glb_fp = lib_dir ++ "\\_globals.h"
        writeFile glb_fp oup_glbs
  lib_dirs <- D.getSubPaths ("examples\\" ++ D.oArch os ++ "\\libs")
  mapM_ processLib lib_dirs


-- should generalize the top one to call this one
collectLibrarySampleIsaFromDir :: D.Opts -> FilePath -> IO ()
collectLibrarySampleIsaFromDir os_raw full_dll = cudaSdk12 -- oldBody
  where os = os_raw{D.oPrintLines = True, D.oPrintOffsets = True, D.oPrintEncoding = True}

        cudaSdk12 :: IO ()
        cudaSdk12 = do
          -- now --extract-elf doesn't work
          -- we need to extra all sm_90 elfs and process each one by one instead
          -- (may be faster any way)
          putStrLn $ "============= DUMPING " ++ full_dll
          cuod_exe <- D.findCudaTool os "cuobjdump"
          let output_dir = "examples/" ++ D.oArch os ++ "/libs/" ++ takeFileName (dropExtension full_dll)

          -- --extract-elf .sm_90.cubin
          out_elf <- readProcess cuod_exe ["--extract-elf","." ++  D.oArch os ++ ".cubin",full_dll] ""
          -- output is:
          -- Extracting ELF file    8: tmpxft_0000653c_00000000-7.sm_90.cubin
          -- Extracting ELF file   48: tmpxft_0000653c_00000000-47.sm_90.cubin
          let cubins_files = map (last . words) (lines out_elf)

          createDirectoryIfMissing True output_dir

          let processCubinElf :: String -> IO ()
              processCubinElf elf_cubin = do
                putStrLn $ "  " ++ elf_cubin ++ ": processing"
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
                -- "tmpxft_0000653c_00000000-47.sm_90.cubin" => "Program-47.sm_90"
                let cubinToProgramName :: String -> String
                    cubinToProgramName = ("Elf" ++) . dropExtension . dropWhile (/='-')

                    prog_oup = cubinToProgramName elf_cubin

                putStrLn $ "  % nva ... filtering assembly output"
                D.runWithOpts
                  os {
                    D.oInputFile = tmp_sass
                  , D.oOutputFile = output_dir ++ "/" ++ prog_oup ++ ".sass"
                  , D.oFilterAssembly = True
                  }
                renameFile elf_cubin (output_dir ++ "/" ++ prog_oup ++ ".cubin")
                removeFile tmp_sass

          mapM_ processCubinElf cubins_files

          unless (null cubins_files) $ do
            out_ptx <- readProcess cuod_exe ["--extract-ptx","." ++  D.oArch os ++ ".ptx",full_dll] ""
            print out_ptx
            -- output is:
            -- Extracting PTX file and ptxas options    1: Program-NVIDIA-GPU-Computing-nppial64_12.1.sm_90.ptx -arch=sm_90
            -- Extracting PTX file and ptxas options    2: Program.2.sm_90.ptx -arch=sm_90
            -- ..
            -- Extracting PTX file and ptxas options   39: Program.39.sm_90.ptx -arch=sm_90 -maxrregcount=40
            -- let ptx_files = map (last . init . words) (lines out_ptx)
            let ptx_files = map ((!! 7) . words) (lines out_ptx)

                processPtxFile :: String -> IO ()
                processPtxFile ptx_file = do
                  putStrLn $ "  " ++ ptx_file ++ ": moving"
                  renameFile ptx_file (output_dir ++ "/" ++ ptx_file)

            writeFile (output_dir ++ "/ptx.opts") out_ptx

            mapM_ processPtxFile ptx_files


        oldBody :: IO ()
        oldBody = do
          putStrLn $ "============= DUMPING " ++ full_dll
          cuod_exe <- D.findCudaTool os "cuobjdump"
          out <- readProcess cuod_exe ["--list-elf",full_dll] ""
          let elfs_lines = filter (("." ++ D.oArch os ++ ".")`isInfixOf`) (lines out)
          putStrLn $ "  found " ++ show (length elfs_lines) ++ " symbols matching arch"
          let output_dir = "examples/" ++ D.oArch os ++ "/libs/" ++ takeFileName (dropExtension full_dll)
          createDirectoryIfMissing True output_dir
          mapM_ (processElfOld cuod_exe output_dir) (map (last . words) elfs_lines)
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
                    (proc cuod_exe ["--dump-ptx",full_dll]) {std_out = UseHandle h}
              (Nothing,Nothing,Nothing,ph) <- createProcess cp
              ec <- waitForProcess ph
              putStrLn $ "  ptx dumping => " ++ show ec

        processElfOld :: FilePath -> FilePath -> String -> IO ()
        processElfOld cuod_exe output_dir elf_cubin = do
          putStrLn $ "  dumping lib cubin " ++ elf_cubin
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
              , D.oFilterAssembly = True
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
