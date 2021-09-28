module NVT.SASSGraph where

import NVT.IR
import NVT.Dataflow
import NVT.Parsers.ListingParser

import Control.Monad
import Data.Char
import Data.List

emitTextSection :: TextSection -> IO ()
emitTextSection ts =
--  writeFile (tsKernelName ts ++ ".gvz") $
--    fmtTextSect ts
  putStrLn $
    tsKernelName ts ++ ":\n" ++
    concatMap (\b -> "B#" ++ show (bId b) ++ ":\n" ++
        concatMap (\i -> "  I#" ++ show (iId i) ++ "\n") (bInsts b)
      ) (tsBlocks ts)
--
-- digraph sample3 {
-- A -> {B ; C ; D}
-- C -> {B ; A}
-- }

fmtTextSect :: TextSection -> String
fmtTextSect ts = ""
