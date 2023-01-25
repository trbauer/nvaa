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
          -- when (null lib_root_sass_files) $
          --   putStrLn "warning: no matching files found"
          mapM_ processF lib_root_sass_files
          return ()

processFile :: FilePath -> IO Bool
processFile sass_fp = do
  putStrLn $ "============================ " ++ sass_fp
  sass_inp <- readFile sass_fp
  (z,kis) <- D.testListing sass_fp sass_inp
  forM_ kis $ \(knm,blens) -> do
    putStrLn $ knm ++ ": " ++ show blens
  return z


blaze :: IO ()
blaze = do
  -- processFile "toy-files90\\asyncAPI.sass"
  -- processFile "toy-files90\\bitonic.sass"
  -- processFile "toy-files90\\bodysystemcuda.sass"
  -- processFile "toy-files90\\Mandelbrot_cuda.sass"
  -- processFile "toy-files90\\matrixMul.sass"
  processFile "toy-files90\\simpleAWBarrier.sass"
  --
  -- processFile "toy-files90\\globalToShmemAsyncCopy.sass"
  -- processFile "toy-files90\\simpleSeparateCompilation.sass"
  return ()

fixme :: IO ()
fixme = do
  -- FIXED => D.testInst "IADD3     R14,    R28,    -0x30,  RZ               {!1};"
--  D.testInst $
--    "  /*0000*/       ULDC.64       UR1,     cx[UR5][UR10+0x28]                         {!8};  /* 000FF00000000800`00000A00FF017B82 */"
  D.testInst $
    -- 000FE200000001FF`00000000FF197435
    -- "HFMA2.MMA  R25,  -RZ,  RZ,  1,  1  {!1};"
    -- " /*6800*/ @!P3  FMUL      R24,    R24,    4296                     {!4,Y,^3};  /* 004FC80000400000`458000001818B820 */"
    -- "/*01D0*/       FENCE.VIEW.ASYNC.S  {!10,+1.W};  /* 000E340000000000`00000000000073C6 */"
    -- "SYNCS.EXCH.64  UR7, [UR4], UR10 {!2,+1.W,+1.R}; // 000E620008500027`00000010FF1079A7 examples/sm_90/samples/0_Introduction/simpleAWBarrier/simpleAWBarrier.sass:2682"
    -- "SYNCS.ARRIVE.TRANS64.ART0  R20, [UR39], R16 {!1,+2.W}; // 000E620008500027`00000010FF1079A7 examples/sm_90/samples/0_Introduction/simpleAWBarrier/simpleAWBarrier.sass:2787"
    -- "BRA.DIV   UR4,  `(.L_x_15)  {!5};  /* 000FEA000B800000`0000001604B07947 */"
    -- "BRX       R42 -0x7cb0  {!5};"
    -- "BRX !P6, R12 -0xb50 {!5};"
    -- "WARPSYNC.COLLECTIVE  R15, `(.L_x_48)  {!1};"
    -- "BAR.SYNC  R13,    R13   {!6,+2.R};"
    -- "P2R       R28,    PR,     RZ,     0x1              {!9,Y};"
    "P2R.B1    R175,   PR,     R172,   0xf              {!2};"
  return ()



