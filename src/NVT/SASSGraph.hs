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
      concatMap (\b -> "B#" ++ show (bId b) ++ ": " ++ intercalate "," (bLabels b) ++ "\n" ++
          concatMap (\i -> "  I#" ++ show (iId i) ++ ": " ++ format i ++ "\n") (bInsts b)
        ) (tsBlocks ts) ++ "\n" ++
      show idg ++ "\n" ++
      "\n" ++
      df_str

  where idg = computePreds (tsBlocks ts)
        dus = computeDUs (tsBlocks ts)
        df_str =
          "DUs:\n" ++
          concatMap (\du -> "  " ++ show du ++ "\n") dus

--
-- digraph sample3 {
-- A -> {B ; C ; D}
-- C -> {B ; A}
-- }

{-
fmtTextSect :: TextSection -> String
fmtTextSect ts =
    "digraph " ++ tsKernelName ts ++ "{\n" ++

    "}"
  where idg = computePreds (tsBlocks ts)
-}