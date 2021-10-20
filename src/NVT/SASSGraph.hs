module NVT.SASSGraph where

import NVT.IR
import NVT.Dataflow
import NVT.Parsers.ListingParser

import Control.Monad
import Data.Char
import Data.List
import Debug.Trace
import qualified Data.IntSet as DIS
import qualified Data.IntMap.Strict as DIM


emitTextSection :: TextSection -> IO ()
emitTextSection ts = do
--  writeFile (tsKernelName ts ++ ".gvz") $
--    fmtTextSect ts
    putStrLn $
      tsKernelName ts ++ ":\n" ++
      concatMap (\b -> "B#" ++ show (bId b) ++ ": " ++ intercalate "," (bLabels b) ++ "\n" ++
          concatMap (\i -> "  I#" ++ show (iId i) ++ ": " ++ format i ++ "\n") (bInsts b)
        ) (tsBlocks ts) ++ "\n" ++
      -- show idg ++ "\n" ++
      "\n" ++
      df_str
    writeFile "out.dot" $
      fmtTsDot ts

  where idg = bsPreds (tsBlocks ts)
        dus = bsDefUses (tsBlocks ts)

        df_str =
          "DUs:\n" ++
          concatMap (\du -> "  " ++ show du ++ "\n") dus ++
          "END"

fmtTsDot :: TextSection -> String
fmtTsDot ts =
    "digraph G {\n" ++
    "  rankdir = TB;\n" ++
    "\n" ++
    concatMap fmtB (tsBlocks ts) ++
    "\n" ++
    concatMap fmtBTerms (tsBlocks ts) ++
    "\n" ++
    concatMap fmtDu dus ++
    "}\n"
  where fmtB :: Block -> String
        fmtB b =
            "  subgraph B" ++ show (bId b) ++ " {\n" ++
            "    color=black\n" ++
            "    b" ++ show (bId b) ++ " [label=" ++ b_lbl ++ "]\n" ++
            concatMap fmtInstDecl (bInsts b) ++
            first_inst ++
            loopInsts (bInsts b) ++
            "  }\n"
          where first_inst :: String
                first_inst =
                  case bInsts b of
                    [] -> ""
                    i:_ -> "    b" ++ show (bId b) ++ "->i" ++ show (iId i) ++ "\n"
                b_lbl :: String
                b_lbl =
                  case bLabels b of
                    [] -> "B#" ++ show (bId b)
                    (lbl:_) -> show lbl
                loopInsts :: [Inst] -> String
                loopInsts [] = ""
                loopInsts [_] = ""
                loopInsts (i0:i1:is) =
                  "    i" ++ show (iId i0) ++ "->i" ++ show (iId i1) ++ " [style=dashed]\n" ++
                  loopInsts (i1:is)

        dus :: [DU]
        dus = bsDefUses (tsBlocks ts)

        fmtDu :: DU -> String
        fmtDu du =
          "  i" ++ show (duDef du) ++ "->i" ++ show (duUse du) ++ "\n"

        fmtInstDecl :: Inst -> String
        fmtInstDecl i =
            "    i" ++ show (iId i) ++ " [style=filled,shape=box,color=" ++
            color ++ ",label=\"#" ++ show (iId i) ++ ":" ++ format (iOp i) ++ "\"]\n"
          where color
                  | iOp i `elem` flt_ops = "\"#FFFFCC\""
                  | iOp i `elem` int_ops = "\"#CCFFCC\""
                  | iOp i `elem` buf_ops = "\"#FFCCCC\""
                  | otherwise = "\"#CCCCCC\""

        fmtInstEdges :: Block -> String
        fmtInstEdges b = first ++ loop (bInsts b)
          where first =
                  case bInsts b of
                    [] -> ""
                    (i0:_) -> "    b" ++ show (bId b) ++ "->i" ++ show (iId i0) ++ " [style=dashed]\n"

                loop [] = ""
                loop [_] = ""
                loop (i0:i1:is) =
                  "    i" ++ show (iId i0) ++ "->" ++
                    "i" ++ show (iId i1) ++ "\n" ++
                  loop (i1:is)

        fmtBTerms :: Block -> String
        fmtBTerms b
          | null (bInsts b) = ""
          | otherwise = concatMap fmtEdgeTo dst_edges
          where fmtEdgeTo :: Int -> String
                fmtEdgeTo b_succ_id = "  i" ++ show (iId (last (bInsts b))) ++ "->b" ++ show b_succ_id ++ "\n"

                dst_edges :: [Int]
                dst_edges =
                  case bId b`DIM.lookup`b_succs of
                    Nothing -> []
                    Just iset -> DIS.toList iset

        b_succs :: IDGraph
        b_succs = bsSuccs (tsBlocks ts)


flt_ops :: [Op]
flt_ops = [
  OpF2F,OpF2FP,OpFMUL,OpFFMA,
  OpFADD,OpFRND,
  OpIMAD,
  OpHADD2,OpHFMA2,OpHMUL2]

int_ops :: [Op]
int_ops = [
  OpI2I,OpIADD3,OpISETP,OpIMNMX,
  OpFLO,OpFMNMX,OpFSETP,OpFSET,OpFCHK,
  OpLEA,OpLOP3,OpMOV,
  OpPRMT,
  OpSEL,OpSHF
  ]

buf_ops :: [Op]
buf_ops = [
  OpLDGSTS,OpLDGDEPBAR,OpLDSM,
  OpLD,OpLDG,OpLDL,OpLDS,
  OpST,OpSTG,OpSTS,OpSTL,
  OpATOM,OpATOMG,OpATOMS,
  OpQSPC,
  OpSUST,
  OpTEX,OpTLD
  ]

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
