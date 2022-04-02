module Main where

import NVT.Driver
import NVT.CUDASDK
import NVT.Opts
import NVT.RawInst
import NVT.Bits

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf
import qualified Data.Map.Strict as DM

-- goal of the study
-- frequency of LEA in programs
-- XXX run through all the collected instructions (group from histogram summarys and sources)
-- run through all the listings/programs

-- emit CSV with
--   count for LEA vs others


sm_ver :: String
sm_ver = "sm_86"
-- sm_ver = "sm_80"
-- sm_ver = "sm_75"

main :: IO ()
main = do
  as <- getArgs
  case as of
    [] -> collectOps
    _
      | any (`elem`as) ["-h","--help"] ->
        putStrLn "usage: study_opfreq.exe"
      | otherwise -> die "unexpected option"


collectOps :: IO ()
collectOps = body
  where body = do
          -- for all .sass files
          --    for all lines in each file
          --       tryParseFilteredRawInst line
          --       split base opcode off
          --       appendFile ("examples/" ++ base_op ++ "/") $
          -- createDirectoryIfMissing True output_root
          --
--          samples_sass_files <-  filterSassOnly <$> getSubPaths "examples/sm_75/samples"
--          mapM_ processFile (samples_sass_files)
          --
          -- library_dirs <- getSubPaths "examples/sm_75/libs" >>= filterM doesDirectoryExist
          -- libraries_sass_files <- filterSassOnly . concat <$> mapM getSubPaths library_dirs
          -- mapM_ processFile library_dirs
          let processDir :: FilePath -> IO ()
              processDir dir = do
                let filterSassOnly = filter (".sass"`isSuffixOf`)
                lib_root_sass_files <- filterSassOnly <$> getSubPaths dir
                mapM_ processFile lib_root_sass_files
                return ()
          --
          let key =
                ["Workload","TOTAL OPS",""] ++
                ["OTHER","LEA","ULEA"] ++ [""] ++
                ["OTHER %","LEA %","ULEA %"]
          writeFile output_file $ intercalate "," key ++ "\n"
          --
          let smpls_path = "examples/" ++ sm_ver ++ "/samples"
          processDir smpls_path
          --
--          let lib_path = "examples/" ++ sm_ver ++ "/libs"
--          lib_dirs <- getSubPaths lib_path
--          mapM_ processDir lib_dirs

        output_file :: FilePath
        output_file = "opfreq.csv"

        processFile :: FilePath -> IO ()
        processFile fp = do
          putStrLn $ "==== collecting ops from " ++ fp
          flns <- lines <$> readFile fp
          -- flns <- zip [1..] . lines <$> readFile fp
          let ops = foldl' sampleLine DM.empty flns
          if DM.null ops then putStrLn "skipping (0 ops)"
            else
              appendFile output_file $
                fmtOpsLine (dropExtension (takeFileName fp)) ops ++ "\n"

        sampleLine :: DM.Map String Int -> String -> DM.Map String Int
        sampleLine ops lnstr =
            case parseSampleInst lnstr of
              Left _ -> ops
              Right si -> DM.insertWith (+) (riMnemonic (siRawInst si)) 1 ops


fmtOpsLine :: String -> DM.Map String Int -> String
fmtOpsLine file ops = intercalate "," $
    [file,show n_ops,""] ++ map show [n_ops - n_lea - n_ulea,n_lea,n_ulea] ++
    [""] ++ map fmtPct [n_ops - n_lea - n_ulea,n_lea,n_ulea]
  where fmtPct :: Int -> String
        fmtPct n = printf "%6.3f" (100.0*n_d/n_total)
          where n_d = fromIntegral n :: Double
                n_total = fromIntegral n_ops :: Double
        n_ops = sum (DM.elems ops)
        n_lea =
          case "LEA"`DM.lookup`ops of {Nothing -> 0; Just x -> x}
        n_ulea =
          case "ULEA"`DM.lookup`ops of {Nothing -> 0; Just x -> x}



