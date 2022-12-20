module CollectDUs where

import qualified NVT.CUDASDK as D
import qualified NVT.Diagnostic as D
import qualified NVT.Driver as D
import qualified NVT.IR as D
import qualified NVT.Opts as D
import qualified NVT.ListingTypes as D
import qualified NVT.Parsers.ListingParser as D


import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Data.IORef
import Data.List
import Debug.Trace
import System.FilePath
import System.Environment
import System.Exit
import System.Directory
import System.IO
import System.Process
import Text.Printf

-- import qualified Data.Map.Strict as DM
-- import qualified Data.ByteString as S



dft_opts :: D.Opts
-- dft_opts = D.dft_opts_86
dft_opts = D.dft_opts_90 -- sm_90

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run as = parseDUOpts duo_dft as >>= runWithOpts

fatal :: String -> IO a
fatal = die

data DUOpts =
  DUOpts {
    duoFilters :: ![String]
  , duoParallelism :: !Int
  , duoClobber :: !Bool
  , duoFailFast :: !Bool
  , duoVerbosity :: !Int
  } deriving Show
duo_dft :: DUOpts
duo_dft =
  DUOpts {
    duoFilters = []
  , duoParallelism = 1
  , duoClobber = False
  , duoFailFast = True
  , duoVerbosity = 0
  }
duoMatchesFilter :: DUOpts -> String -> Bool
duoMatchesFilter duo str =
  null (duoFilters duo) || any (`isInfixOf`str) (duoFilters duo)

parseDUOpts :: DUOpts -> [String] -> IO DUOpts
parseDUOpts duo [] = return duo
parseDUOpts duo (a:as)
  | a `elem` ["-h","--help"] = do
    putStrLn $
      "usage:  collect_dus  OPTS  FILTERS\n" ++
      "  where OPTS are:\n" ++
      "     -c            clobber\n" ++
      "     -f            fail fast\n" ++
      "     -j[=INT]      parallelism\n" ++
      "     -v[=INT]      verbosity\n" ++
      "  and FILTERS are a list of infix matches to include\n" ++
      ""
    exitSuccess
  | a `elem` ["-t"] = badArg ("expected " ++ a ++ "=...")
  | a == "-c" = parseDUOpts (duo{duoClobber = True}) as
  | a`elem`["-f","--fail-fast"]  = parseDUOpts (duo{duoFailFast = True}) as
  | a == "-j" = parseDUOpts (duo{duoParallelism = 8}) as
  | "-j="`isPrefixOf`a = handleIntArg (\x -> duo{duoParallelism = x})
  | a == "-v" = parseDUOpts (duo{duoVerbosity = 1}) as
  | "-v="`isPrefixOf`a = handleIntArg (\x -> duo{duoVerbosity = x})
  | "-"`isPrefixOf`a = badArg "unrecognized option"
  | otherwise = parseDUOpts (duo{duoFilters = duoFilters duo ++ [a]}) as
  where badArg msg = fatal $ a ++ ": " ++ msg
        eq_val = drop 1 . dropWhile (/='=') $ a
        handleIntArg func =
          case reads eq_val of
            [(x,"")] -> parseDUOpts (func x) as


runWithOpts :: DUOpts -> IO ()
runWithOpts duo = do
    createDirectoryIfMissing True output_root
    --
    let smpls_path = "examples\\" ++ sm_arch ++ "\\samples"
    smpls_dirs <- concat <$> (D.getSubPaths smpls_path >>= mapM D.getSubPaths)
    mapM_ processDir smpls_dirs
    --
    let lib_path = "examples\\" ++ sm_arch ++ "\\libs"
    lib_dirs <- D.getSubPaths lib_path
    mapM_ processDir lib_dirs
    return ()

  where sm_arch :: String
        sm_arch = D.oArch dft_opts

        output_root :: FilePath
        output_root = "examples/" ++ sm_arch ++ "/ops"

        processDir :: FilePath -> IO ()
        processDir dir = do
          let filterSassOnly = filter (".sass"`isSuffixOf`)
          lib_root_sass_files <- filterSassOnly <$> D.getSubPaths dir
          let processF sass_fp
                | not (duoMatchesFilter duo sass_fp) = return ()
                | otherwise = do
                  z <- processFile sass_fp
                  when (not z && duoFailFast duo) $ exitFailure
          mapM_ processF lib_root_sass_files
          return ()

processFile :: FilePath -> IO Bool
processFile sass_fp = do
  putStrLn $ "============================ " ++ sass_fp
  sass_inp <- readFile sass_fp
  let sass_lns = lines sass_inp
  case D.parseListing sass_fp sass_inp of
    Left d -> do
      hPutStrLn stderr (D.dFormatWithLines sass_lns d)
      return False
    Right l -> do
      putStrLn "parsed"
      let emitB b = do
            print (b {D.bInsts = []})
            let fmtI i =
                  D.fmtInstIr i ++ "\n" ++
                  "INP:  " ++ (sass_lns !! (D.lLine (D.iLoc i) - 1)) ++ "\n" ++
                  "FMT:  " ++ D.format i ++ "\n"
            mapM_ (putStr . fmtI) (D.bInsts b)
      let emitTs ts = do
            print $ ts {D.tsBlocks = []}
            mapM_ emitB (D.tsBlocks ts)
      mapM_ emitTs (D.lTextSections l)
      return True


fixme :: IO ()
fixme = do
  -- FIXED => D.testInst "IADD3     R14,    R28,    -0x30,  RZ               {!1};"
  return ()



