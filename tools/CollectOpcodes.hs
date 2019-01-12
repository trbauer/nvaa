module CollectOpcodes where

import NVT.Driver
import NVT.CUDASDK
import NVT.Opts
import NVT.RawInst
import NVT.Word128

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
--          samples_sass_files <-  filterSassOnly <$> getSubPaths "examples/sm_75/samples"
--          mapM_ processFile (samples_sass_files)
          libraries_sass_files <- filterSassOnly <$> getSubPaths "examples/sm_75/libs"
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
            case parseRawInstWithBits lnstr of
              Nothing -> return ()
              Just ri -> do
                let syntax =
                      printf "%016X`%016X:  " (wHi64 (riBits ri)) (wLo64 (riBits ri)) ++
                      fmtRawInstWithOpts True ri
                    source_info = fp ++ ":" ++ show lno
                let base_op = takeWhile (/='.') (riMnemonic ri)
                appendFile (output_root ++ "/" ++ base_op ++ ".sass") $
                   printf "%-80s" syntax ++ " // " ++ source_info ++ "\n"


-- want to infer the bit patterns of all opcodes
-- for each opcode X
--   load all samples (with bits)
--   find the longest common field starting from 0 where all samples share the same value
--

-- QUERIES:
--
--  * SAMPLE FIELDS
--    given fields Fs, list the values in a list of samples
--             [15:10]    [27]
--     SYN1    0x4        ...
--     SYN2    0x4        ...
--     SYN3    0x4        ...
--     SYN4    0x4        ...
--     SYN5    0x4        ...
--
--
--   * TWIDDLE VALUES
--   Given a base instruction word W, enumerate syntax as we try all
--   values for fields Fs
--    [15:10]       [27]
--    0b000000 (0)    0     SYNTAX
--    0b000000 (0)    1     *** CRASH *** one-line msg
--    0b000001 (1)    0     SYNTAX
--
-- For each format
--   *
