module NVT.Dataflow where

import NVT.IR
import NVT.ListingTypes

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

fmtRegSet :: RegSet -> String
fmtRegSet rs
  | DS.null rs = "{ }"
  | otherwise = "{" ++ intercalate "," (map format (DS.toList rs)) ++ "}"


bsDefUses :: [Block] -> [DU]
bsDefUses bs = sortOn duDef (iterate ist0)
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
        prds = bsPreds bs

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

-- type IDGraph = DIM.IntMap DIS.IntSet

bsSuccs :: [Block] -> IDGraph
bsSuccs bs = foldl' acc DIM.empty pairs
  where idg = bsPreds bs

        acc :: IDGraph -> (Int,Int) -> IDGraph
        acc m (b_prd,b_suc) = DIM.insertWith DIS.union b_prd (DIS.singleton b_suc) m

        pairs :: [(Int,Int)]
        pairs = concatMap (\(b_succ,b_prds) -> map (\b_prd -> (b_prd,b_succ)) (DIS.toList b_prds)) (DIM.toList idg)


bsPreds :: [Block] -> IDGraph
bsPreds bs = itr ps0 bs
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
        ins = toRegSet (pr ++ srcs)

        toRegSet :: [Reg] -> RegSet
        toRegSet = DS.fromList . filter (not .  isZReg)

        isZReg :: Reg -> Bool
        isZReg xr =
          case xr of
            RegR RZ -> True
            RegUR URZ -> True
            RegP PT -> True
            RegUP UPT -> True
            _ -> False

        pr :: [Reg]
        pr =
          case iPredication i of
            PredP _ PT -> []
            PredUP _ UPT -> []
            PredP _ p -> [RegP p]
            PredUP _ up -> [RegUP up]

        srcs :: [Reg]
        srcs = concatMap acc (iSrcs i)
          where acc :: Src -> [Reg]
                acc s =
                  case s of
                    SrcReg _ r@(RegR _)
                      | is_st_or_at && iHasInstOpt InstOpt64 i -> regSeq 2 r
                      | is_st_or_at && iHasInstOpt InstOpt128 i -> regSeq 4 r
                    SrcReg _ r
                      | oIsD (iOp i) -> regSeq 2 r
                      | otherwise -> [r]
                    SrcAddr (r,r_ms) ur _ -> regSeq nr (RegR r) ++ [RegUR ur]
                      where nr = if Mod64`msElem`r_ms then 2 else 1
                    SrcDescAddr ur (r,r_ms) _ ->
                        regSeq nr (RegR r) ++ regSeq 2 (RegUR ur)
                      where nr = if Mod64`msElem`r_ms then 2 else 1
                    SrcCon _ (SurfReg ur) _ -> [RegUR ur]
                    _ -> []

        is_st_or_at :: Bool
        is_st_or_at = oIsAT (iOp i) || oIsST (iOp i)
        is_ld_or_at :: Bool
        is_ld_or_at = oIsAT (iOp i) || oIsLD (iOp i)

        ous :: RegSet
        ous = toRegSet $ concatMap acc (iDsts i)
          where acc :: Dst -> [Reg]
                acc (Dst _ r@(RegR _)) = regSeq nregs r -- data
                acc (Dst _ r) = regSeq 1 r -- things like predicates

                nregs :: Int
                nregs
                  | oIsD (iOp i) = 2
                  | is_ld_or_at && iHasInstOpt InstOpt64 i = 2
                  | is_ld_or_at && iHasInstOpt InstOpt128 i = 4
                  | otherwise = 1
                  where is_ld = oIsLD (iOp i) || oIsAT (iOp i)

        regSeq :: Int -> Reg -> [Reg]
        regSeq 0 _ = []
        regSeq i r =
            case r of
              RegR RZ -> []
              RegR r -> handle RegR r
              RegP PT -> []
              RegP pr -> handle RegP pr
              RegUR URZ -> []
              RegUR ur -> handle RegUR ur
              RegUP UPT -> []
              RegUP up -> handle RegUP up
              --
              RegB b -> handle RegB b
              RegSB sb -> handle RegSB sb
              RegSR sr -> handle RegSR sr
          where handle :: (Eq r,Enum r) => (r -> Reg) -> r -> [Reg]
                handle c xr =
                  case eMaybeNext xr of
                    Nothing -> []
                    Just xr1 -> r : regSeq (i - 1) (c xr1)

eMaybeNext :: (Eq a,Enum a) => a -> Maybe a
eMaybeNext a
  | a == lst = Nothing
  | otherwise = Just (toEnum (fromEnum a + 1))
  where lst = last [toEnum 0 ..]
