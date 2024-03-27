module Main where
  -- {-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.Char
import Data.List
import Data.IORef
import Data.Typeable
import Data.Word
import Debug.Trace
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Printf
import qualified Data.Set as DS
import qualified Data.Map.Strict as DM
import qualified System.Console.ANSI as SCA -- cabal install ansi-terminal

-- TODO: abort later targets if dependent target fails
-- TODO: honor config files in dir (instead of .cu)
--       to allow for multiple targets in a directory
-- TODO: -q hides everything except errors
--       -v=0 shows only modified changes
--       -v=1 shows success steps (e.g. skipped)
-- TODO: -j=... parallelism for parallel build
-- TODO: sniff out #include dependencies...
-- TODO: cmake should be pseudo target
--        - store in options oCmake :: [(FilePath,Arch)]
--        - why not treat it like a regular target (non-default)?
-- TODO: clean should be a pseudo target (which allows no SM)
--        - oClean :: [(FilePath,[ArtifactKind],Arch)]
--          should clean just clean all targets?
-- TODO: the graph logic is not robust
--     A `targ(X)` B
--     B `targ(X)` C
--    IF B is stale, then so is C
--    I suspect I only check locally in the run.
--    Since, we run serially, we are probably safe.
--
-- TODO: Eliminate up to date targets from the build plan

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run as = parseOpts dft_opts as >>= runWithOpts

data Opts =
  Opts {
    oArtifacts :: ![Artifact]
  , oClean :: !Bool -- clean mode
  , oClobber :: !Bool
  , oCmake :: !Bool
  , oDryRun :: !Bool
  , oFailFast :: !Bool
  , oKeep :: !Bool
  , oKeepInlines :: !Bool
  , oVerbosity :: !Int
  , oWatch :: !(Maybe Float)
  , oXnvcc :: ![String]
  , oXnvdisasm :: ![String]
  } deriving (Show,Eq)

dft_opts :: Opts
dft_opts =
  Opts {
    oArtifacts = []
  , oClean = False
  , oClobber = False
  , oCmake = False
  , oDryRun = False
  , oFailFast = True
  , oKeep = False
  , oKeepInlines = True
  , oVerbosity = 0
  , oWatch = Nothing
  , oXnvcc = []
  , oXnvdisasm = []
  }

-- relative to given directory
mINCU_PATH :: FilePath
mINCU_PATH = "../mincu.hpp"

type Artifact = (ArtifactKind,FilePath,String)
data ArtifactKind =
    ArtifactKindEXE
  | ArtifactKindSASS
  | ArtifactKindSASSG -- dot graph
  | ArtifactKindSASSP -- dot graph
  | ArtifactKindPTX
  | ArtifactKindCUBIN
  deriving (Eq,Show,Ord,Enum)
all_artifact_kinds :: [ArtifactKind]
all_artifact_kinds = [a0 ..] where a0 = toEnum 0
dft_artifact_kinds :: [ArtifactKind]
dft_artifact_kinds = [ArtifactKindEXE,ArtifactKindSASS,ArtifactKindPTX]

fmtArtifactKind :: ArtifactKind -> String
fmtArtifactKind = drop (length "ArtifactKind") . map toLower . show
fmtArtifact :: Artifact -> String
fmtArtifact (ak,path,sm) = path ++ ":" ++ fmtArtifactKind ak ++ ":" ++ sm


parseOpts :: Opts -> [String] -> IO Opts
parseOpts os [] = do
  when (null (oArtifacts os)) $
    fatal "artifact expected (try -h)"
  when (oClean os && oCmake os) $
    fatal "--clean and --cmake are mutually exclusive"
  return os
parseOpts os (a:as)
  | a `elem` ["-h","--help"] = do
    putStrLn $
      "usage: bexp OPTS TARGET\n" ++
      "Build Experiment\n" ++
      "where OPTS are:\n" ++
      "   -c/--clobber               overrides file time checks\n" ++
      "      --clean                 remove outputs from given targets\n" ++
      "      --cmake                 setup cmake files and a VS solution for all target/architecture pairs\n" ++
      "                              (clobber stomps old files here)\n" ++
      "   -d/--dry-run               don't actually execute the operation\n" ++
      "   -F/--no-fail-fast          don't fail fast\n" ++
      "   -k/--keep                  keep intermediate files (e.g. raw asm)\n" ++
      "   --keep-inlines             keeps inline info in SASS\n" ++
      "   -q/-v/-v2/-v=INT           sets the verbosity\n" ++
      "      --setup                 list/check setup\n" ++
      "   -Xnvcc=..                  extra option to nvcc (can specify multiple times)\n" ++
      "   -Xnvdisasm=..              extra option to nvdisasm (can specify multiple times)\n" ++
      "   -w/--watch=SECONDS         enable watch mode (iteratively builds)\n" ++
      "where TARGETs are of the form:\n" ++
      "  FILE(:(" ++ intercalate "|" (map fmtArtifactKind all_artifact_kinds) ++ "))?(SM,SM,...)\n" ++
      "  SM = two digits (e.g. \"90\" would indicate sm_90)\n" ++
      "    (last two digits from entries of nvcc --list-gpu-arch)\n" ++
      "  If artifacts are absent; a default set is chosen to build.\n" ++
      "  If FILE is '.' (current dir), then it matches all .cu files.\n" ++
      "EXAMPLES:\n" ++
--      "  % bexp  foo.cu\n" ++
      "  % bexp  bar.cu:exe:75\n" ++
      "    builds exe for sm_75 for file bar.cu\n" ++
      "  % bexp  bar.cu::90\n" ++
      "    builds all artifacts of bar.cu for sm_90\n" ++
      "  % bexp foo.cu:sass90 -Xnvcc=-rdc=true\n" ++
      "    builds SASS code for sm_90 adding -rdc=true in the nvcc steps\n" ++
      "  % bexp  bar.cu::75 --clean\n" ++
      "    cleans all artifacts of bar.cu for sm_75\n" ++
      "  % bexp  foo.cu::75 bar.cu::75,86 --cmake\n" ++
      "    Sets up cmake for foo.cu for sm_75 and bar.cu for sm_75 and sm_86\n" ++
      "  % bexp  .::72,75 --cmake\n" ++
      "    Set up cmake for *.cu for sm_72 and sm_75\n" ++
      ""
    exitSuccess
  --
  | a == "-q" = nextArg os{oVerbosity = -1}
  | a == "-v" = nextArg os{oVerbosity = 1}
  | a `elem` ["-v2"] = nextArg os{oVerbosity = 2}
  | k == "-v=" = parseAsIntValue (\i -> os{oVerbosity = i})
  --
  | a `elem` ["--setup"] = checkTools True >> exitSuccess
  --
  | a `elem` ["--cmake"] = nextArg os {oCmake = True}
  | a `elem` ["--cmake"] = nextArg os {oCmake = True}
  | a `elem` ["-d","--dry-run"] = nextArg os {oDryRun = True}
  | a `elem` ["-F","--no-fail-fast"] = nextArg os {oFailFast = False}
  | a `elem` ["-c","--clobber"] = nextArg os {oClobber = True}
  | a `elem` ["-k","--keep"] = nextArg os {oKeep = True}
  | a `elem` ["--keep-inlines"] = nextArg os {oKeepInlines = True}
  | k `elem` ["-w=","--watch"] = parseAsFloatValue (\f -> os{oWatch = Just f})
  | k `elem` ["-Xnvcc="] = nextArg os {oXnvcc = oXnvcc os ++ [v]}
  | k `elem` ["-Xnvdisasm="] = nextArg os {oXnvdisasm = oXnvdisasm os ++ [v]}
  --
  | "-"`isPrefixOf`a = badArg "invalid option"
  --
  | otherwise = do
    as <- parseTargets a
    nextArg os {oArtifacts = oArtifacts os ++ as}
  where (k,v) =
          case span (/='=') a of
            (k,'=':sfx) -> (k ++ "=",sfx)
            (inp,"") -> (inp,"")

        nextArg :: Opts -> IO Opts
        nextArg os = parseOpts os as

        parseTargets  :: String -> IO [Artifact]
        parseTargets a =
            case splitOn ':' a of
              [path_strs,targ_strs,arch_strs] -> do
                ps <- parsePaths path_strs
                aks <- parseArtifactKinds targ_strs
                archs <- parseArchs arch_strs
                return $ complete ps aks archs
              _ -> badArg "malformed command; should be of form: FILE:(TARG(,TARG)*)?:SM_DIGITS(,SM_DIGITS)*"

          where complete :: [FilePath] -> [ArtifactKind] -> [String] -> [Artifact]
                complete paths aks archs =
                   [(ak,path,arch) | path<-paths, ak<-aks, arch<-archs]

                parsePaths :: FilePath -> IO [FilePath]
                parsePaths "." = do
                  es <- listDirectory "."
                  return $ filter (".cu"`isSuffixOf`) es
                parsePaths path = do
                  when (any (`elem`"/\\") path) $
                    putStrYellow $ "WARNING: should be run from within local experiment directory\n"
                  file_exists <- doesFileExist path
                  (path,file_exists) <-
                    if file_exists || ".cu"`isSuffixOf`path then return (path,file_exists)
                      else do
                        file_exists <- doesFileExist (path ++ ".cu")
                        return (path ++ ".cu",file_exists) -- error takes more specific path
                  if not file_exists
                    then badArg $ path ++ ": file not found"
                    else return [path]

                parseArtifactKinds :: String -> IO [ArtifactKind]
                parseArtifactKinds "" = return dft_artifact_kinds
                parseArtifactKinds s = loop [] (splitOnCommas s)
                  where loop :: [ArtifactKind] -> [String] -> IO [ArtifactKind]
                        loop raks [] = return (reverse raks)
                        loop raks (s:ss) =
                          case find (\a -> fmtArtifactKind a == s) all_artifact_kinds of
                            Just ak -> loop (ak:raks) ss
                            Nothing -> badArg $ s ++ ": unrecognized artifact"

                parseArchs :: String -> IO [String]
                parseArchs s = do
                    case find shadyLooking strs of
                      Nothing -> return ()
                      Just s ->
                        badArg $
                          s ++ ": suspicious looking SM architecture " ++
                          "(e.g. expecting something like 75, 90, or 90a)"
                    return strs
                  where strs = splitOnCommas s
                        shadyLooking = not . all isDigit . take 2


        parseAsIntValue :: (Int -> Opts) -> IO Opts
        parseAsIntValue func =
          case reads v of
            [(x,"")] -> parseOpts (func x) as
            _ -> badArg "malformed integer"
        parseAsFloatValue :: (Float -> Opts) -> IO Opts
        parseAsFloatValue func =
          case reads v of
            [(x,"")] -> parseOpts (func x) as
            _ -> badArg "malformed float"

        badArg :: String -> IO a
        badArg msg = fatal (a ++ ": " ++ msg)

splitOnCommas :: String -> [String]
splitOnCommas = splitOn ','
splitOn :: Char -> String -> [String]
-- splitOn s = words . map (\c -> if c == s then ' ' else c)
splitOn sep = loop ""
  where loop rcurr [] = [reverse rcurr]
        loop rcurr (c:cs)
          | c == sep = reverse rcurr:loop "" cs
          | otherwise = loop (c:rcurr) cs

oNormalLn :: Opts -> String -> IO ()
oNormalLn = oNormalLnH stdout
oVerboseLn :: Opts -> String -> IO ()
oVerboseLn = oVerboseLnH stdout
oDebugLn :: Opts -> String -> IO ()
oDebugLn = oDebugLnH stdout
oNormalLnH :: Handle -> Opts -> String -> IO ()
oNormalLnH h = oIoLevel (hPutStrLn h) 0
oVerboseLnH :: Handle -> Opts -> String -> IO ()
oVerboseLnH h = oIoLevel (hPutStrLn h) 1
oDebugLnH :: Handle -> Opts -> String -> IO ()
oDebugLnH h = oIoLevel (hPutStrLn h) 2
oIoLevel :: (String -> IO ()) -> Int -> Opts -> String -> IO ()
oIoLevel io lvl os
  | oVerbosity os >= lvl = io
  | otherwise = const (return ())


fatal :: String -> IO a
fatal = die

checkTools :: Bool -> IO ()
checkTools vrb = do
  let handler_cl :: SomeException -> IO ()
      handler_cl _ = do
        fatal $ "cannot find cl.exe in %PATH%.  Did you run setupvs.bat (or vcvarsall.bat)?"
        return ()
  -- frickin cl.exe emits a banner to stderr!
  (readProcessWithExitCode "cl.exe" [] "" >> return ()) `catch` handler_cl

  let handler_nva :: SomeException -> IO ()
      handler_nva se = do
        fatal $ "cannot find nva.exe in %PATH%"
  (readProcess "nva.exe" ["-h"] "" >> return ()) `catch` handler_nva
  let handler_nvcc :: SomeException -> IO ()
      handler_nvcc se = do
        fatal $ "cannot find nvcc.exe in %PATH%"
  (readProcess "nvcc.exe" ["--version"] "" >> return ()) `catch` handler_nva

  when vrb $ do
    let checkTool exe = do
          putStr $  "=== " ++ printf "%-24s" exe
          mpath <- findExecutable exe
          case mpath of
            Just exe -> putStrGreen $ exe
            Nothing -> putStrRed " ???"
          putStrLn ""
    checkTool "cmake.exe"
    checkTool "cl.exe"
    checkTool "nvcc.exe"
    checkTool "nva.exe"

runWithOpts :: Opts -> IO ()
runWithOpts os
  | oClean os = do
    forM_  (oArtifacts os) $ \a -> do
      oVerboseLn os $ "cleaning " ++ fmtArtifact a
      (_,oups) <- a_inputs_outputs True a
      forM_ oups $ \oup -> do
        z <- doesFileExist oup
        when z $ do
          oDebugLn os $ "  removing " ++ oup
          removeFile oup

  | oCmake os = do
    z <- doesFileExist "CMakeLists.txt"
    when (z && not (oClobber os)) $
      fatal "CMakeLists.txt already exists in this directory"
    --
    let exe_archs = nub $ map (\(_,fp,sm) -> (fp,sm)) (oArtifacts os)
        addExe (cu_file,sm) =
            -- TODO: sniff out headers (and conditionally include mincu as well)
            "add_executable(" ++ show exe_prj ++ " " ++ cu_file ++ " " ++ mINCU_PATH ++ ")\n" ++
            "target_compile_features(" ++ show exe_prj ++ " PUBLIC cxx_std_20)\n" ++
            "set_target_properties(" ++ show exe_prj ++ " PROPERTIES CUDA_SEPARABLE_COMPILATION ON)\n" ++
            -- "set(CMAKE_CUDA_FLAGS ${CMAKE_CUDA_FLAGS} \"-g -G\")  # enable cuda-gdb\n" ++
            "set_target_properties(" ++ show exe_prj ++ " PROPERTIES CUDA_ARCHITECTURES " ++ show sm ++ ")\n" ++
            "target_include_directories(" ++ show exe_prj ++ " PUBLIC \"" ++ takeDirectory mINCU_PATH ++ "\")\n" ++
            ""
          where exe_prj = dropExtension (takeFileName cu_file) ++ sm
        vs_solution_name =
          -- if it's single target (multiple archs okay), then use that
          -- otherwise use this directory name
          case nub (map fst exe_archs) of
            [x] -> dropExtension x -- use the single cu-file
            x:_ -> takeFileName (takeDirectory x)
    --
    writeFile "CMakeLists.txt" $
      "cmake_minimum_required(VERSION 3.25.2) # CMAKE_CUDA_STANDARD 20 for C++20\n" ++
      "project(" ++ show vs_solution_name ++ " LANGUAGES CXX CUDA)\n" ++
      "\n" ++
      "set(CUDA_NVCC_FLAGS \"${CUDA_NVCC_FLAGS}\" \"-g\" \"-G\")\n" ++
      "set(CMAKE_CUDA_STANDARD 20)\n" ++
      "set(CMAKE_CUDA_STANDARD_REQUIRED ON)\n" ++
      "\n" ++
      intercalate "\n\n" (map addExe exe_archs)
    --
    -- run cmake
    let cmake_dir = "vs"
    z <- doesDirectoryExist cmake_dir
    when (z && not (oClobber os)) $
      fatal $ cmake_dir ++ ": cmake build dir already exists"
    when z $ do
      oDebugLn os $ "nuking " ++ cmake_dir
      removeDirectoryRecursive cmake_dir
    --
    createDirectoryIfMissing True cmake_dir
    let cp =
          (proc "cmake" ["-G","Visual Studio 17 2022","-A","x64",".."]) {
            cwd  = Just cmake_dir
          }
    (Nothing,Nothing,Nothing,ph) <- createProcess cp
    ec <- waitForProcess ph
    case ec of
      ExitSuccess -> oDebugLn os $ "cmake exited 0"
      ExitFailure ec -> fatal $ "cmake exited " ++ show ec

  | otherwise = do -- mapM_ (runCommandWithOpts os) (oCommands os)
    let p = makePlan (oArtifacts os)
    when (oVerbosity os >= 2) $ do
      putStrLn "============= PLAN =============="
      forM_ (zip [0..] p) $ \(ix,as) ->
        putStrLn $ "step " ++ show ix ++ ": " ++ intercalate "|" (map fmtArtifact as)
    unless (oDryRun os) $ checkTools (oVerbosity os >= 2)
    let loopForUpdate :: [Artifact] -> IO Bool
        loopForUpdate [] = return False
        loopForUpdate (a:as) = do
          sr <- shouldRebuild os a
          case sr of
            ShouldRebuildERROR err -> fatal (fmtArtifact a ++ ": " ++ err)
            ShouldRebuildUPTODATE -> loopForUpdate as
            x -> do
              putStrLn $ "============= " ++ fmtArtifact a ++ ": " ++ show x
              return True
    let watchForUpdate delay_s = do
          when (delay_s > 0.0) $ do
            threadDelay (round (delay_s * 1000 * 1000))
          when (oVerbosity os >= 2) $
            putStrLn "checking for updates"
          z <- loopForUpdate (concat p)
          when z $ executePlan os p
          watchForUpdate delay_s
    --
    executePlan os p
    case oWatch os of
      Nothing -> return ()
      Just s -> watchForUpdate s

-- list of concurrent plans we can run
-- EXE
-- PTX -> CUBIN
-- SASS -> CUBIN
-- [[PTX|SASS],[CUBIN,EXE]]
makePlan :: [Artifact] -> [[Artifact]]
makePlan as =
    build_ordering DS.empty [] $ collect_implicit (DS.fromList as) as
  where collect_implicit :: DS.Set Artifact -> [Artifact] -> [Artifact]
        collect_implicit s [] = DS.toList s
        collect_implicit s (a:as) = collect_implicit s1 as
          where s1 = s`DS.union`DS.fromList (a_dependents_tc a)

        build_ordering :: DS.Set Artifact -> [[Artifact]] -> [Artifact] -> [[Artifact]]
        build_ordering s_done rsteps [] = reverse rsteps
        build_ordering s_done rsteps as =
            -- find all those with no previously unhandled step
            case partition a_ready as of
              ([],_) -> error "INTERNAL ERROR: circular dependency in artifacts"
              (this_phase,as_rest) ->
                  build_ordering s_done1 ((sort this_phase):rsteps) as_rest
                where s_done1 = s_done`DS.union`DS.fromList this_phase
            where a_ready :: Artifact -> Bool
                  a_ready a = all (`DS.member`s_done) deps
                      where deps = a_dependents_tc a

a_dependents_tc :: Artifact -> [Artifact]
a_dependents_tc (ak,fp,sm) =
    map (\ak1 -> (ak1,fp,sm)) (dependents_tc ak)
  -- tc is transitive closure
  where dependents_tc :: ArtifactKind -> [ArtifactKind]
        dependents_tc a =
          case a of
            ArtifactKindEXE -> []
            ArtifactKindSASS -> [ArtifactKindCUBIN]
            ArtifactKindSASSG -> [ArtifactKindCUBIN]
            ArtifactKindSASSP -> [ArtifactKindCUBIN]
            ArtifactKindPTX -> []
            ArtifactKindCUBIN -> []

-- (inputs, outputs) of a phase
-- is_for_clean means to include stuff that might not matter like .lib files
--  this is used by --clean, but not incremental rebuild
a_inputs_outputs :: Bool -> Artifact -> IO ([FilePath], [FilePath])
a_inputs_outputs is_for_clean (ak,fp,sm) = do
    uses_mincu <- usesMincuHpp fp
    let maybe_mincu
          | uses_mincu = [mINCU_PATH]
          | otherwise = []
        root_files = [fp] ++ maybe_mincu
    return $
      case ak of
        -- what about mincu.hpp?
        ArtifactKindCUBIN -> (root_files,[cubin_fp])
        ArtifactKindEXE
          | is_for_clean -> (root_files,[exe_fp,exe_lib_fp,exe_exp_fp])
          | otherwise -> (root_files,[exe_fp])
        ArtifactKindPTX -> (root_files,[ptx_fp])
        ArtifactKindSASS -> ([cubin_fp],[sass_fp])
        ArtifactKindSASSP -> ([cubin_fp],[sassp_fp])
        ArtifactKindSASSG -> ([cubin_fp],[sass_png_fp,dot_fp])
  where stem = dropExtension fp
        sass_fp = stem ++ "-sm_" ++ sm ++ ".sass"
        sassp_fp = stem ++ "-ptx-sm_" ++ sm ++ ".sass"
        sass_raw_fp = stem ++ "-raw-sm_" ++ sm ++ ".sass"
        sass_png_fp = stem ++ "-sass-sm_" ++ sm ++ ".png"
        ptx_fp = stem ++ "-sm_" ++ sm ++ ".ptx"
        cubin_fp = stem ++ "-sm_" ++ sm ++ ".cubin"
        exe_fp = stem ++ "-sm_" ++ sm ++ ".exe"
        exe_lib_fp = stem ++ "-sm_" ++ sm ++ ".lib"
        exe_exp_fp = stem ++ "-sm_" ++ sm ++ ".exp"
        dot_fp = stem ++ "-sm_" ++ sm ++ ".dot"

        usesMincuHpp :: FilePath -> IO Bool
        usesMincuHpp fp = do
          flns <- lines <$> readFile fp
          let incl_pfx = ["#include", "\"mincu.hpp\""] -- other crap could follow
          return $ any ((incl_pfx`isPrefixOf`) . words) flns



type BuildMap = DM.Map Artifact TaskResult

executePlan :: Opts -> [[Artifact]] -> IO ()
executePlan os ass = executeStep DM.empty 0 ass
  where executeStep :: BuildMap -> Int -> [[Artifact]] -> IO ()
        executeStep _ _ [] = return ()
        executeStep bm n (as:ass) = do
            when (oVerbosity os >= 1) $
              putStrLn $ "========== executing step " ++ show n ++ " ..."
            trs <- mapM (execSerial bm) as
            let bm1 = foldl' (\bm (a,tr) -> DM.insert a tr bm) bm (zip as trs)
            executeStep bm1 (n + 1) ass

        execSerial :: BuildMap -> Artifact -> IO TaskResult
        execSerial bm a = do
          when (oVerbosity os >= 0) $
            emitTitle a >> hFlush stdout
          (tr,dio) <- executeArtifact os bm a
          emitTaskResult a (tr,dio)
          when (oFailFast os && trFailed tr) $ do
            putStrRed "(failing fast)\n"
            exitFailure
          return tr

        -- execParallel as = do
        --    mvs <- mapM newEmptyMVar [0 .. length as - 1]
        --
        --    mapM_ (forkIO runTask) (zip as mvs)
        --    a_trs <- zip as <$> mapM takeMVar mvs
        --  iterate the results
        --  for each \(a,(tr,dio))
        --    emitTitle a >> emitTaskResult (tr,dio)
        --  if any of the tr is TaskResultERROR, then exitFailure

        emitTitle :: Artifact -> IO ()
        emitTitle a = putStrCyan $ printf "%-48s" (fmtArtifact a ++ ":")

        -- emit the deferred I/O and list the result
        emitTaskResult :: Artifact -> (TaskResult,IORef (IO ())) -> IO ()
        emitTaskResult a (tr,dio) = do
          when (oVerbosity os >= 0 || trFailed tr) $ do
            when (oVerbosity os < 0) $ -- parent won't print it otherwise
              emitTitle a
            case tr of
              TaskResultNOP -> putStrGreen "up to date" >> putStrLn ""
              TaskResultUPDATED msg -> putStrGreen ("success") >> putStrLn maybe_verb
                where maybe_verb
                        | oVerbosity os >= 1 = " (" ++ msg ++ ")"
                        | otherwise = ""
              TaskResultABORTED err -> putStrDarkRed ("aborted: " ++ err) >> putStrLn ""
              TaskResultERROR err -> putStrRed ("error: " ++ err) >> putStrLn ""
              TaskResultINTERNAL err -> putStrDarkRed ("internal error: " ++ err) >> putStrLn ""
          io <- readIORef dio
          io


data TaskException = TaskException !String deriving (Show,Typeable)
instance Exception TaskException

data TaskResult =
    TaskResultNOP
  | TaskResultUPDATED   !String
  | TaskResultABORTED   !String
  | TaskResultERROR     !String
  | TaskResultINTERNAL  !String -- internal error
  deriving (Show,Eq)
trFailed :: TaskResult -> Bool
trFailed tr =
  case tr of
    TaskResultERROR _ -> True
    TaskResultABORTED _ -> True
    TaskResultINTERNAL _ -> True
    _ -> False


executeArtifact :: Opts -> BuildMap -> Artifact -> IO (TaskResult,IORef (IO ()))
executeArtifact os bm a = do
  dio <- newIORef (return ())
  case aFailedPrereqs bm a of
    [] -> do
      let handle_te :: TaskException -> IO TaskResult
          handle_te (TaskException msg) = return $ TaskResultERROR msg
      let handle_other :: SomeException -> IO TaskResult
          handle_other se = return $ TaskResultERROR (show se)
      tr <- executeArtifactWithDefIo os a dio `catch` handle_te `catch` handle_other
      return (tr,dio)
    as -> return (TaskResultABORTED failed,dio)
      where failed = "prereqs failed: " ++ intercalate ", " (map fmtArtifact as)

aFailedPrereqs :: BuildMap -> Artifact -> [Artifact]
aFailedPrereqs bm a =
    filter aFailed (a_dependents_tc a)
  where aFailed a =
          case a`DM.lookup`bm of
            Just tr -> trFailed tr
            Nothing -> error "INTERNAL ERROR: no TaskResult for prereq!"



executeArtifactWithDefIo :: Opts -> Artifact -> IORef (IO ()) -> IO TaskResult
executeArtifactWithDefIo os a dio = do
  sr <- shouldRebuild os a
  let dIO io1 = modifyIORef dio $ \io -> io >> io1
  let runWith :: String -> IO TaskResult
      runWith why = do
        buildArtifact os dio a
        -- TODO: if not no clobber check outputs are created?
        return $ TaskResultUPDATED why
  case sr of
    ShouldRebuildUPTODATE -> return TaskResultNOP
    ShouldRebuildCLOBBER -> runWith "clobber specified"
    ShouldRebuildERROR err -> return $ TaskResultERROR err
    ShouldRebuildNEW fs -> do
      when (oVerbosity os >= 2) $
        dIO $ putStrLn $ "must run to create output file(s): " ++ intercalate ", " fs
      runWith "new output"
    ShouldRebuildSTALE fs -> do
      when (oVerbosity os >= 2) $
        dIO $
          putStrLn $
            "the following files are out of date:\n" ++
            concatMap (\(op,ip) -> op ++ " is older than " ++ ip) fs
      runWith "stale files"


data ShouldRebuild =
    ShouldRebuildUPTODATE
  | ShouldRebuildERROR String -- fatal problem (e.g. input missing)
  | ShouldRebuildCLOBBER -- -c was set
  | ShouldRebuildNEW [FilePath] -- output files missing
  | ShouldRebuildSTALE [(FilePath,FilePath)] -- files are stale
  deriving (Eq,Show)

-- returns Nothing if not
shouldRebuild :: Opts -> Artifact -> IO ShouldRebuild
shouldRebuild os a@(ak,_,_)
  | oClobber os = return $ ShouldRebuildCLOBBER
  | otherwise = do
    (inps,oups) <- a_inputs_outputs False a
    let doesFileNotExist f = not <$> doesFileExist f
    missing_inp <- filterM doesFileNotExist inps
    if not (null missing_inp)
      then return $ ShouldRebuildERROR ("missing input file(s):\n" ++ intercalate ", " missing_inp)
      else do
        missing_oups <- filterM doesFileNotExist oups
        if not (null missing_oups) then return (ShouldRebuildNEW missing_oups)
          else do -- all outputs exist
            let getMts fps = do
                    forM fps $ \fp -> do
                      mt <- getModificationTime fp
                      return (fp,mt)
            oup_mts <- getMts oups
            inp_mts <- getMts inps
            -- show me the list of outputs older than an input
            let findStaleOups o@(oup,omt) =
                    concatMap checkInp inp_mts :: [(FilePath,FilePath)]
                  where checkInp (inp,imt)
                          | imt > omt = [(oup,inp)]
                          | otherwise = []
                all_stale = concatMap findStaleOups oup_mts
            if null all_stale then return ShouldRebuildUPTODATE
              else return $ ShouldRebuildSTALE all_stale


callExeToFile :: Opts -> IORef (IO ()) -> FilePath -> [String] -> FilePath -> String -> IO ()
callExeToFile os dio exe as oup_fp inp = callExeG os dio exe as (Just oup_fp) inp
callExe :: Opts -> IORef (IO ()) -> FilePath -> [String] -> String -> IO ()
callExe os dio exe as inp = callExeG os dio exe as Nothing inp
callExeG :: Opts -> IORef (IO ()) -> FilePath -> [String] -> Maybe FilePath -> String -> IO ()
callExeG os dio exe as m_oup_fp inp = do
  let dIO io1 = modifyIORef dio $ \io -> io >> io1
  let esc_arg a
        | any isSpace a = "\"" ++ a ++ "\""
        | otherwise = a
  when (oVerbosity os >= 2 || oDryRun os) $
    dIO $ do
      putStrLn $ " % " ++ esc_arg exe ++ " " ++ intercalate " " (map esc_arg as)
  unless (oDryRun os) $ do
    (ec,out,err) <- readProcessWithExitCode exe as inp
    case ec of
      ExitSuccess -> do
        when (oVerbosity os >= 2) $
           dIO $ putStrLn ("   >> exited " ++ show ec)
        when (oVerbosity os >= 1 && not (null err)) $
           dIO $ putStrLn err
        case m_oup_fp of
          Just oup_fp -> writeFile oup_fp out
          Nothing -> return ()
      ExitFailure ec -> do
        when (oVerbosity os < 2) $ -- we didn't print it earlier
          dIO $ putStrLn $ " % " ++ esc_arg exe ++ " " ++ intercalate " " (map esc_arg as)
        dIO $ putStrLn $
                "  >> exited " ++ show ec ++ "\n" ++
                "ERR:\n" ++ err ++ "\n" ++
                "OUT:\n" ++ out ++ "\n"
        throwIO $ TaskException (takeFileName exe ++ " failed")

buildArtifact :: Opts -> IORef (IO ()) -> Artifact -> IO ()
buildArtifact os dio a@(ak,fp,arch) = do
  let fp_noext = dropExtension fp
      fp_cubin = fp_noext ++ "-" ++ "sm_" ++ arch ++ ".cubin"
      fp_exe = fp_noext ++ "-" ++ "sm_" ++ arch ++ ".exe"
      fp_rsass = fp_noext ++ "-raw-" ++ "sm_" ++ arch ++ ".sass"
      fp_sass = fp_noext ++ "-" ++ "sm_" ++ arch ++ ".sass"
      fp_sassp = fp_noext ++ "-ptx-" ++ "sm_" ++ arch ++ ".sass"
      fp_dot_sass = fp_noext ++ "-" ++ "sm_" ++ arch ++ ".dot"
      fp_sass_png = fp_noext ++ "-sass-sm_" ++ arch ++".png"
      fp_ptx = fp_noext ++ "-" ++ "sm_" ++ arch ++ ".ptx"
      maybe_opt f o = if f os then [] else [o]

  case ak of
    ArtifactKindEXE -> do
      -- 	nvcc -std=c++20 -arch sm_${EXE_ARCH} -I ../../tools ${WORKLOAD}.cu
      --           -o ${WORKLOAD}${EXE_ARCH}.exe
      callExe os dio "nvcc"
          ([ "-std=c++20"
          , "-arch","sm_"++arch
          , "-I", takeDirectory mINCU_PATH
          , "-o", fp_exe
          , fp
          ] ++ oXnvcc os) ""
    ArtifactKindCUBIN -> do
      callExe os dio "nvcc"
        ([ "-std=c++20"
        , "-arch","sm_"++arch
        , "-I", takeDirectory mINCU_PATH
        , "--generate-line-info"
        -- , "--source-in-ptx" (shouldn't be needed; since PTX doesn't use this path)
        , "-cubin"
        , "-o", fp_cubin
        , fp
        ] ++ oXnvcc os) ""

    ArtifactKindPTX -> do
      callExe os dio "nvcc"
        ([ "-std=c++20"
        , "-arch","sm_"++arch
        , "-I", takeDirectory mINCU_PATH
        , "--generate-line-info" -- .loc directives
        , "--source-in-ptx" -- emits // lines with source (requires --generate-line-info)
        , "--ptx"
        , "-o", fp_ptx
        , fp
        ] ++ oXnvcc os) ""
    ArtifactKindSASS -> do
      callExeToFile os dio "nvdisasm"
        ([ "--print-instruction-encoding" -- for depinfo
        -- , "--print-line-info-ptx" -- use sassp
        , "--print-line-info" -- for lines
        , "--print-line-info-inline" -- for inline call sites
        -- , "--print-life-ranges"
        , "--print-code" -- only text sections
        , fp_cubin
        ] ++ oXnvdisasm os) fp_rsass ""
        -- just for filter assembly
      callExe os dio "nva.exe"
        ([ "--arch=sm_" ++ arch
        , "-lines"
        ] ++ maybe_opt oKeepInlines "-Xinlined" ++
        [
          fp_rsass
        , "-o=" ++ fp_sass
        ]) ""
      unless (oKeep os) $
        removeFile fp_rsass

    ArtifactKindSASSP -> do
      callExeToFile os dio "nvdisasm"
        [ "--print-instruction-encoding" -- for depinfo
        , "--print-line-info-ptx"
        , "--print-line-info" -- for lines
        , "--print-line-info-inline" -- for inline call sites
        -- , "--print-life-ranges"
        -- , "--print-code" -- only text sections (needed for PTX)
        , fp_cubin
        ] fp_rsass ""
        -- just for filter assembly
      callExe os dio "nva.exe"
        ([ "--arch=sm_" ++ arch
        , "-lines"
        ] ++ maybe_opt oKeepInlines "-Xinlined" ++
        [
          fp_rsass
        , "-o=" ++ fp_sassp
        ]) ""
      unless (oKeep os) $
        removeFile fp_rsass

    ArtifactKindSASSG -> do
      callExeToFile os dio "nvdisasm"
        ([ "--print-instruction-encoding" -- for depinfo
        , "--print-line-info" -- for lines
        , "--print-line-info-inline" -- for inline call sites
        , "--print-code" -- only text sections
        , "-cfg" -- for dot input
        , fp_cubin
        ] ++ oXnvdisasm os) fp_dot_sass ""
      callExe os dio "dot"
        [
          "-Tpng"
        , "-o" ++ fp_sass_png
        , fp_dot_sass
        ] ""



-- nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu --generate-line-info --ptx -I .. -arch sm_${ARCH}
-- # nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu --generate-line-info --ptx --source-in-ptx -I .. -arch sm_${ARCH}
-- mv ${WORKLOAD}.ptx ${WORKLOAD}-sm_${ARCH}.ptx

putStrRed :: String -> IO ()
putStrRed = hPutVivid SCA.Red stdout
putStrDarkRed :: String -> IO ()
putStrDarkRed = hPutDull SCA.Red stdout
putStrGreen :: String -> IO ()
putStrGreen = hPutVivid SCA.Green stdout
putStrYellow :: String -> IO ()
putStrYellow = hPutVivid SCA.Yellow stdout
putStrCyan :: String -> IO ()
putStrCyan = hPutVivid SCA.Cyan stdout
--
hPutVivid :: SCA.Color -> Handle -> String -> IO ()
hPutVivid = hPutColor SCA.Vivid
hPutDull :: SCA.Color -> Handle -> String -> IO ()
hPutDull = hPutColor SCA.Dull
hPutColor :: SCA.ColorIntensity -> SCA.Color -> Handle -> String -> IO ()
hPutColor i c h str = bracket_ acq rel act
  where acq = hFlush h >> SCA.hSetSGR h [SCA.SetColor SCA.Foreground i c]
        act = hPutStr h str
        rel = SCA.hSetSGR h [SCA.Reset]
