module NVT.Dataflow where

import NVT.IR
import NVT.Parsers.ListingParser

import Data.Char
import Data.List
import Debug.Trace
import qualified Data.Set as DS
import qualified Data.IntSet as DIS
import qualified Data.Map.Strict as DM
import qualified Data.IntMap.Strict as DIM



-- liveSet :: LiveSet -> DIM.IntMapLiveSet

-- map inst PC to uses that target it
-- DIM.IntMap

-- blockid => DuSet

type RegSet = DS.Set Reg

type IDGraph = DIM.IntMap DIS.IntSet

data DU =
  DU {
    duDef :: !Int
  , duUse :: !Int
  -- duPred :: ... the predicate def that presents this
  , duReg :: !Reg
--  , duMinDist :: !Int
--  , duBlocks :: !DIS.IntSet
  } deriving (Show,Eq)

data DUAnalysis =
  DUAnalysis {
    duS :: ![DU]
  , duLiveIn :: ![(Int,Reg)]
  , duDead :: ![(Int,Reg)]
  , duLiveOut :: ![(Int,Reg)]
  } deriving (Show,Eq)


computeDUs :: [Block] -> [DU]
computeDUs bs = sortOn duDef (iterate ist0)
  where rbs = reverse (map (\b -> b{bInsts = reverse (bInsts b)}) bs)
        ist0 =
          ItrSt {
            istLiveOuts = DIM.fromList $ map (\b -> (bId b,DM.empty)) rbs
          , istItr = 0
          , istDus = []
          , istCopyOut = False
          , istChanged = True
          }

        prds :: IDGraph
        prds = computePreds bs

        iterate :: ItrSt -> [DU]
        iterate ist
          | istChanged ist = iterate $ iterateBlocks (ist {istItr = istItr ist + 1,istChanged = False})
          | otherwise = istDus (iterateBlocks (ist {istCopyOut = True}))

        iterateBlocks :: ItrSt -> ItrSt
        iterateBlocks ist0 = foldl' accB ist0 rbs
          where accB ist b =
                  case bId b`DIM.lookup`istLiveOuts ist of
                    Just b_live_out -> ist1 {istDus = rdus ++ istDus ist}
                      where (b_new_live_in,rdus) =
                              iterateBlock (istCopyOut ist) b_live_out b
                            ist1 = -- trace (debugB b b_new_live_in)  $
                              case bId b`DIM.lookup`prds of
                                Just prds -> foldl' propBack ist (DIS.toList prds)
                                  where propBack :: ItrSt -> Int -> ItrSt
                                        propBack ist b_pid =
                                            case b_pid`DIM.lookup`istLiveOuts ist of
                                              Just b_plps ->
                                                  ist {
                                                    istLiveOuts = DIM.insert b_pid b_plps1 (istLiveOuts ist)
                                                  , istChanged = istChanged ist || b_plps /= b_plps1
                                                  }
                                                where b_plps1 :: LivePaths
                                                      b_plps1 =
                                                        DM.unionWith DIS.union b_plps b_new_live_in

        -- debugB :: Block -> LivePaths -> String
        -- debugB b lps =
        --   "B#" ++ show (bId b) ++ ": " ++ intercalate ", " (bLabels b) ++ "\n" ++
        --   concatMap (\(r,is) -> "  " ++ format r ++ "=>" ++ intercalate ", " (map (\i -> "I#" ++ show i) (DIS.toList is)) ++ "\n") (DM.toList lps)

        iterateBlock :: Bool -> LivePaths -> Block -> (LivePaths,[DU])
        iterateBlock copy_out live0 b = loopInst [] DM.empty live0 (bInsts b)
          where loopInst :: [DU] -> PredKills -> LivePaths -> [Inst] -> (LivePaths,[DU])
                loopInst rdus _ live [] = (live,rdus)
                loopInst rdus pks live (i:ris) =
                    case foldl' remOu (rdus,live,pks) ous of
                      (new_rdus,new_live,new_pks) ->
                          loopInst new_rdus new_pks new_new_live ris
                        where new_new_live = foldl' addIn new_live (DS.toList ins)
                  where (ins,ous) = depsInpsOups i

                        remOu :: ([DU],LivePaths,PredKills) -> Reg -> ([DU],LivePaths,PredKills)
                        remOu (rdus,live,pks) r =
                            case iPredication i of
                              -- unpredicated kill
                              PredP ps PT -> (mkNewDus False,DM.delete r live,DM.delete r pks)
                              PredUP ps UPT -> (mkNewDus False,DM.delete r live,DM.delete r pks)
                              -- predicated kill
                              p -> (mkNewDus True,new_live,new_pks)
                                where (new_live,new_pks) =
                                        case r`DM.lookup`pks of
                                          -- this is the first predicated kill
                                          --   no change to live (not definitely killed)
                                          --   add the predicated kill
                                          Nothing -> (live,DM.insertWith DS.union r (DS.singleton p) pks)
                                          -- successive kill
                                          Just pks_prds
                                            -- completed the kill
                                            | compl p`DS.member`pks_prds -> (DM.delete r live,DM.delete r pks)
                                            --
                                            -- rekill under non-complementary predicate
                                            --  @P1 MOV  R10, ... <<< HERE
                                            --  @P0 MOV  R10, ... <<< or HERE (@P0 already present)
                                            --  @P0 MOV  R10, ...
                                            --  ... use R10
                                            | otherwise -> (live,DM.insertWith DS.union r (DS.singleton p) pks)

                          where mkNewDus :: Bool -> [DU]
                                mkNewDus prd
                                  | copy_out =
                                    case r`DM.lookup`live of
                                      Just dis -> map mkDU (DIS.toList dis) ++ rdus
                                      -- TODO: record dead instruction
                                      _ -> rdus
                                  | otherwise = rdus
                                      -- TODO: predication could record set of instructions that define the pred
                                  where mkDU u = DU {duDef = iId i,duUse = u,duReg = r}

                                compl :: Pred -> Pred
                                compl pr =
                                  case pr of
                                    PredP PredPOS p -> PredP PredNEG p
                                    PredP PredNEG p -> PredP PredPOS p
                                    PredUP PredPOS q -> PredUP PredNEG q
                                    PredUP PredNEG q -> PredUP PredPOS q

                        addIn :: LivePaths -> Reg -> LivePaths
                        addIn live r = DM.insertWith DIS.union r (DIS.singleton (iId i)) live



type PredKills = DM.Map Reg (DS.Set Pred)
-- map register to set
--    reg -> set of insts that is currently using it
type LivePaths = DM.Map Reg InstSet
type InstSet = DIS.IntSet

data ItrSt =
  ItrSt {
    -- block to the live DU set
    istLiveOuts :: !(DIM.IntMap LivePaths)
  , istItr :: !Int
  , istDus :: ![DU]
  , istCopyOut :: !Bool
  , istChanged :: !Bool
  } deriving Show

computePreds :: [Block] -> IDGraph
computePreds bs = itr ps0 bs
  where ps0 = DIM.fromList $ map (\b -> (bId b,DIS.empty)) bs

        lbl_map :: DM.Map String Int
        lbl_map = DM.fromList $ concatMap (\b -> map (\s -> (s,bId b)) (bLabels b)) bs

        itr :: IDGraph -> [Block] -> IDGraph
        itr ps [] = ps
        itr ps (b:bs) =
            case bInsts b of
              [] -> itr (addFallthrough ps) bs -- empty block
              is@(_:_) ->
                case last is of
                  i
                    | oIsBranch (iOp i) ->
                      case iPredication i of
                        PredP _ PT -> itr ps_with_brs bs -- unpredicated jump
                        PredUP _ UPT -> itr ps_with_brs bs -- unpredicated jump
                        _ -> itr (addFallthrough ps_with_brs) bs -- predicated (means fallthrough)
                    | otherwise -> itr (addFallthrough ps) bs -- not branch (fallthrough)
                    where ps_with_brs = foldl' acc ps (iSrcs i)

                          acc :: IDGraph -> Src -> IDGraph
                          acc ps src =
                            case src of
                              SrcImmExpr _ (LExprLabel _ lbl) ->
                                case lbl`DM.lookup`lbl_map of
                                  Just target_id -> DIM.insertWith DIS.union target_id (DIS.singleton (bId b)) ps
                                  _ -> ps
                              _ -> ps

          where addFallthrough :: IDGraph -> IDGraph
                addFallthrough ps =
                  case bs of
                    [] -> ps
                    b1:_ -> linkToPred (bId b1) (bId b) ps

                linkToPred :: Int -> Int -> IDGraph -> IDGraph
                linkToPred suc pre ps = DIM.insertWith DIS.union suc (DIS.singleton pre) ps

depsInpsOups :: Inst -> (RegSet,RegSet)
depsInpsOups i = (ins,ous)
  where ins :: RegSet
        ins = DS.fromList $ pr ++ srcs

        pr =
          case iPredication i of
            PredP _ PT -> []
            PredUP _ UPT -> []
            PredP _ p -> [RegP p]
            PredUP _ p -> [RegUP p]

        srcs = concatMap acc (iSrcs i)
          where acc :: Src -> [Reg]
                acc s =
                  case s of
                    SrcReg _ r -> [r]
                    SrcCon _ (SurfReg ur) _ -> [RegUR ur]
                    _ -> []

        ous = DS.fromList $ concatMap acc (iDsts i)
          where acc :: Dst -> [Reg]
                acc (Dst _ r) = [r]


