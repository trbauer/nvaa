module CollectOpcodes where

import NVT.Driver
import NVT.CUDASDK
import NVT.Opts
import NVT.RawInst
import NVT.Bits

import Control.Exception
import Control.Monad
import Data.List
import System.FilePath
import System.Directory
import System.Process
import Text.Printf
-- import qualified Data.Map.Strict as DM
import qualified Data.ByteString as S


collectOps :: IO ()
collectOps = body
  where body = do
          -- for all .sass files
          --    for all lines in each file
          --       tryParseFilteredRawInst line
          --       split base opcode off
          --       appendFile ("examples/" ++ base_op ++ "/") $
          --
          --
          createDirectoryIfMissing True output_root
          let filterSassOnly = filter (".sass"`isSuffixOf`)
          --
          samples_sass_files <-  filterSassOnly <$> getSubPaths "examples/sm_75/samples"
          mapM_ processFile (samples_sass_files)
          --
          library_dirs <- getSubPaths "examples/sm_75/libs" >>= filterM doesDirectoryExist
          libraries_sass_files <- filterSassOnly . concat <$> mapM getSubPaths library_dirs
          mapM_ processFile (libraries_sass_files)
          return ()

        output_root = "examples/sm_75/ops"

        processFile :: FilePath -> IO ()
        processFile fp = do
          putStrLn $ "==== collecting ops from " ++ fp
          flns <- zip [1..] . lines <$> readFile fp
          mapM_ (uncurry (processLine fp)) flns

        processLine :: FilePath -> Int -> String -> IO ()
        processLine fp lno lnstr =
            case parseSampleInst lnstr of
              Left _ -> return ()
              Right si -> do
                let syntax =
                      printf "%016X`%016X:  " (wHi64 (siBits si)) (wLo64 (siBits si)) ++
                      fmtRawInst (siRawInst si)
                    source_info = fp ++ ":" ++ show lno
                let base_op = takeWhile (/='.') (riMnemonic (siRawInst si))
                appendFile (output_root ++ "/" ++ base_op ++ ".sass") $
                   printf "%-80s" syntax ++ " // " ++ source_info ++ "\n"

