module CollectOpcodes where

import NVA.RawInst
import NVT.Driver(getSubPaths)

import Control.Exception
import Control.Monad
import Data.List
import System.FilePath
import System.Directory
import System.Process
-- import qualified Data.Map.Strict as DM
import qualified Data.ByteString as S


collect :: FilePath -> IO ()
collect fp = do
  -- for all .sass files
  --    for all lines in each file
  --       tryParseFilteredRawInst line
  --       split base opcode off
  --       appendFile ("examples/" ++ base_op ++ "/") $
  --
  --     printf "%016X`%016X:  " (wHi64 (riBits ri)) (wLo64 (riBits ri)) ++
  --       fmtRawInstWithOpts False ++
  --       " //" ++ source_info ++ "\n"
  --
  let output_root = "examples/ops"
  createDirectoryIfMissing True output_root
  samples_sass_files <- filter (".sass"`isSuffixOf`) <$> getSubPaths "examples/sm_75/samples"
  libraries_sass_files <- filter (".sass"`isSuffixOf`) <$> getSubPaths "examples/sm_75/libs"
  return ()

