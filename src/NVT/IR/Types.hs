{-# LANGUAGE PatternSynonyms #-}
module NVT.IR.Types where


import NVT.Bits
import NVT.Encoders.Codable
import NVT.IR.Op
import NVT.IR.LExpr
import NVT.IR.Syntax
import NVT.Loc
import qualified NVT.EnumSet as ES

import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Word
import Debug.Trace
import Text.Printf
import qualified Data.Map.Strict as DM

-- TODO: consider more specific IR
--
-- data Srcs =
--     SrcRRR !RegR !RegR  !RegR
--   | SrcRCR !RegR !SrcC  !RegR
--   | SrcRIR !RegR !Imm32 !RegR
--   | SrcRIR !RegR !SrcU  !RegR
--   ...
-- (store extra predicates separate)
--


type PC = Int
type LabelIndex = [(String,PC)]

{-
newtype LabelMap = LabelMap (DM.Map String PC)
  deriving (Show,Eq)

lmEmpty :: LabelMap
lmEmpty

lmElem :: String -> LabelMap -> Bool
lmElem s (LabelMap m) =
-}

data Inst =
  Inst {
    iLoc :: !Loc
  , iId :: !Int
  , iPc :: !PC
  , iPredication :: !Pred
  , iOp :: !Op
  , iOptions :: !InstOptSet -- the mnemonic '.' suffixes
  , iDsts :: ![Dst]
  -- iDstPred :: !(Maybe PR) (or use PT?)
  , iSrcs :: ![Src]
  , iDepInfo :: !DepInfo
  } deriving (Show,Eq)

iLogicallyEqual :: Inst -> Inst -> Bool
iLogicallyEqual i0 i1 = null (iDiffs i0 i1)

-- returns [(field,(old,new))]
iDiffs :: Inst -> Inst -> [(String,(String,String))]
iDiffs i0 i1 =
    concat
      [
        check "iPredication" iPredication
      , check "iOp" iOp
      , check "iOptions" iOptions
      , checkL "iDsts" iDsts
      , checkLG eqSrc "iSrcs" iSrcs
      , check "iDepInfo" iDepInfo
      ]
  where check :: (Eq a,Show a) => String -> (Inst -> a) -> [(String,(String,String))]
        check nm f
          | f i0 == f i1 = []
          | otherwise = [(nm,(show (f i0),show (f i1)))]

        checkL :: (Eq a,Show a) => String -> (Inst -> [a]) -> [(String,(String,String))]
        checkL = checkLG (==)

        checkLG :: (Show a) => (a -> a -> Bool) -> String -> (Inst -> [a]) -> [(String,(String,String))]
        checkLG eq nm f = loop (0 :: Int) (f i0) (f i1)
          where loop _  [] [] = []
                loop ix [] (b:bs) = (nm ++ "[" ++ show ix ++ "]",("<ABSENT>",show b)):loop (ix + 1) [] bs
                loop ix (a:as) [] = (nm ++ "[" ++ show ix ++ "]",(show a,"<ABSENT>")):loop (ix + 1) as []
                loop ix (a:as) (b:bs)
                  | a`eq`b = loop (ix + 1) as bs
                  | otherwise = (nm ++ "[" ++ show ix ++ "]",(show a,show b)):loop (ix + 1) as bs

        eqSrc :: Src -> Src -> Bool
        eqSrc sa sb =
            case (sa,sb) of
              (SrcImmExpr _ le1,SrcImmExpr _ le2) -> rml le1 == rml le2
              _ -> sa == sb
          where rml :: LExpr -> LExpr
                rml le =
                  case le of
                    LExprAdd _ e1 e2 -> LExprAdd lNONE (rml e1) (rml e2)
                    LExprSub _ e1 e2 -> LExprSub lNONE (rml e1) (rml e2)
                    LExprMul _ e1 e2 -> LExprMul lNONE (rml e1) (rml e2)
                    LExprDiv _ e1 e2 -> LExprDiv lNONE (rml e1) (rml e2)
                    LExprMod _ e1 e2 -> LExprMod lNONE (rml e1) (rml e2)
                    LExprNeg _ e -> LExprNeg lNONE (rml e)
                    LExprCompl _ e -> LExprCompl lNONE (rml e)
                    LExprImm _ i -> LExprImm lNONE i
                    LExprLabel _ s -> LExprLabel lNONE s
                    LExprLo32 _ e -> LExprLo32 lNONE (rml e)
                    LExprHi32 _ e -> LExprHi32 lNONE (rml e)
                    LExprSRel _ x e -> LExprSRel lNONE x (rml e)
                    LExprFunFDesc _ e -> LExprFunFDesc lNONE (rml e)
                    LExprGrp _ e -> LExprGrp lNONE (rml e)


iHasInstOpt :: InstOpt -> Inst -> Bool
iHasInstOpt io = (io`ES.member`) . iOptions
iLacksInstOpt :: InstOpt -> Inst -> Bool
iLacksInstOpt io = not . iHasInstOpt io

fmtInstIr :: Inst -> String
fmtInstIr i =
    "Inst {\n" ++
    r (fS "iLoc" iLoc) ++
    fS "iId" iId ++
    fS "iPc" iPc ++
    fS "iPredication" iPredication ++
    fS "iOp" iOp ++
    fS "iOptions" iOptions ++
    ", iDsts = " ++ dst_vals ++
    ", iSrcs = " ++ src_vals ++
    -- fS "iSrcs" iSrcs ++
    fS "iDepInfo" iDepInfo ++
    "}"
  where fS :: Show a => String -> (Inst -> a) -> String
        fS = f show
        f :: (a -> String) -> String -> (Inst -> a) -> String
        f fmt nm prj =
          printf ", %-12s" nm ++ " = " ++ fmt (prj i) ++ "\n"

        r "" = ""
        r (',':sfx) = ' ':sfx

        dst_vals :: String
        dst_vals = "[" ++ intercalate "," (map showD (iDsts i)) ++ "]"

        src_vals :: String
        src_vals
          | length short_str < 64 = "[" ++ short_str ++ "]\n"
          | otherwise =
              case iSrcs i of
                src0:srcs_sfx ->
                  "[\n" ++
                  "    " ++ show src0 ++ "\n" ++
                  concatMap ((++ "\n") . ("  , " ++) . showS) srcs_sfx ++
                  "  ]\n"
          where short_str = intercalate "," (map showS (iSrcs i))

        showD :: Dst -> String
        showD d =
          case d of
            DstRZ -> "DstRZ"
            DstR r -> "DstR " ++ show r
            DstPT -> "DstPT"
            DstP p -> "DstP " ++ show p
            DstURZ -> "DstURZ"
            DstUR ur -> "DstUR " ++ show ur
            DstUPT -> "DstUPT"
            DstUP up -> "DstUP " ++ show up
            _ -> show d

        showS :: Src -> String
        showS s =
          case s of
            SrcRZ -> "SrcRZ"
            SrcR r -> "SrcR " ++ show r
            SrcURZ -> "SrcURZ"
            SrcUR ur -> "SrcUR " ++ show ur

            SrcPT -> "SrcPT"
            SrcP p -> "SrcP " ++ show p
            -- SrcPNT -> "SrcPNT" (can't figure otu the pattern)
            SrcI32 i -> "SrcI32 " ++ show i
            _ -> show s


data Pred =
    --     neg.      rgr.
    PredP  !PredSign !PR -- regular predicate
  | PredUP !PredSign !UP -- uniform predicate (True,UP7) is "UPT"
                         -- this should act the same as PredNONE
  deriving (Show,Eq,Ord)
-- SPECIFY: Pred PredSign PReg
-- SPECIFY: data PReg = PRegPR !PR | PRegUP !UP
instance Syntax Pred where
  format pp =
      case pp of
        PredP z p -> "@" ++ format z ++ format p
        PredUP z p -> "@" ++ format z ++ format p
    where sign z = if z == PredNEG then "!" else ""
data PredSign = PredPOS | PredNEG
   deriving (Show,Eq,Ord,Enum)
instance Syntax PredSign where
  format PredPOS = ""
  format PredNEG = "!"

pred_pt :: Pred
pred_pt = PredP PredPOS PT
pred_upt :: Pred
pred_upt = PredUP PredPOS UPT

data Reg =
    RegR !R -- general register
  | RegP !PR -- predicate register
  | RegB !BR -- convergence barrier register (used by BSYNC, BSSY, BREAK)
  | RegUR !UR -- uniform register
  | RegUP !UP -- uniform predicate
  | RegSB !SB -- (scoreboard register?) (used in DEPBAR)
  | RegSR !SR -- system?/special? register (S2R and CS2R)
  deriving (Show,Eq,Ord)
instance Syntax Reg where
  format reg =
    case reg of
      RegR r -> format r
      RegP r -> format r
      RegB r -> format r
      RegUR r -> format r
      RegUP r -> format r
      RegSB r -> format r
      RegSR r -> format r

pattern RegRZ :: Reg
pattern RegRZ = RegR RZ
pattern RegURZ :: Reg
pattern RegURZ = RegUR URZ
pattern RegPT :: Reg
pattern RegPT = RegP PT
pattern RegUPT :: Reg
pattern RegUPT = RegUP UPT

-- only HSETP2 supports a modifier (.H0_H0)
data Dst = Dst !ModSet !Reg
  deriving (Show,Eq,Ord)
pattern DstRms :: ModSet -> R -> Dst
pattern DstRms ms r = Dst ms (RegR r)
pattern DstR :: R -> Dst
pattern DstR r = Dst ES.EnumSetEMPTY (RegR r)
pattern DstRZ :: Dst
pattern DstRZ = DstR RZ
pattern DstP :: PR -> Dst
pattern DstP p = Dst ES.EnumSetEMPTY (RegP p)
pattern DstPT :: Dst
pattern DstPT = DstP PT
pattern DstB :: BR -> Dst
pattern DstB b = Dst ES.EnumSetEMPTY (RegB b)
pattern DstURZ :: Dst
pattern DstURZ = DstUR URZ
pattern DstUR :: UR -> Dst
pattern DstUR ur = Dst ES.EnumSetEMPTY (RegUR ur)
pattern DstUPT :: Dst
pattern DstUPT = DstUP UPT
pattern DstUP :: UP -> Dst
pattern DstUP up = Dst ES.EnumSetEMPTY (RegUP up)

instance Syntax Dst where
  format (Dst ms r) = msDecorate ms (format r)

data Src =
    SrcReg !ModSet !Reg
  | SrcCon !ModSet !Surf !Reg !Int
  | SrcAddr  !(R,ModSet)  !UR !Int -- [R12.64+UR11-0x10]
  | SrcDescAddr !UR !(R,ModSet)  !Int -- desc[UR14][...]
  | SrcImm !Imm
  | SrcImmExpr !Loc !LExpr
  | SrcImmH2 !Word16 !Word16 -- e.g. HFMA2 ... (1, 1)
  | SrcTex !TexOp
  deriving (Show,Eq,Ord)
pattern SrcRZ :: Src
pattern SrcRZ  = SrcReg ES.EnumSetEMPTY (RegR RZ)
pattern SrcR r  = SrcReg ES.EnumSetEMPTY (RegR r)
pattern SrcURZ = SrcReg ES.EnumSetEMPTY (RegUR URZ)
pattern SrcUR u = SrcReg ES.EnumSetEMPTY (RegUR u)
pattern SrcPT  = SrcReg ES.EnumSetEMPTY (RegP PT)
pattern SrcP p  = SrcReg ES.EnumSetEMPTY (RegP p)
pattern SrcI32 i = SrcImm (Imm32 i)
pattern SrcI8 i = SrcImm (Imm8 i)
--
pattern SrcC0RZX i = SrcCon ES.EnumSetEMPTY (SurfImm 0) RegRZ i
pattern SrcC0URZX i = SrcCon ES.EnumSetEMPTY (SurfImm 0) RegURZ i
--
pattern Src_B ms b = SrcReg ms (RegB b)
pattern Src_P ms p = SrcReg ms (RegP p)
pattern Src_R ms r = SrcReg ms (RegR r)
pattern Src_SB ms r = SrcReg ms (RegSB r)
pattern Src_SR ms r = SrcReg ms (RegSR r)
pattern Src_UP ms up = SrcReg ms (RegUP up)
pattern Src_UR ms ur = SrcReg ms (RegUR ur)
----------------------------------------------------
-- common values
dST_PT :: Dst
dST_PT = DstP PT

dST_UPT :: Dst
dST_UPT = DstUP UPT

sHARED_DSTS :: DM.Map Dst Dst
sHARED_DSTS = DM.fromList $ map (\x -> (x,x)) $
  [
    DstP P0
  , DstP P1
  , DstP P2
  , DstP P3
  , DstP PT
  --
  , DstR R0
  , DstR R1
  , DstR R2
  , DstR R3
  , DstR R4
  , DstR R5
  , DstR R6
  , DstR R7
  --
  , DstUR UR0
  , DstUR UR1
  , DstUR UR2
  , DstUR UR3
  --
  , DstUP UP0
  , DstUP UP1
  , dST_UPT
  ]

dstIntern :: Dst -> Dst
dstIntern = internLookup sHARED_DSTS

sRC_PT :: Src
sRC_PT = SrcReg msEmpty (RegP PT)
--
sRC_NPT :: Src
sRC_NPT = SrcReg (msSingleton ModLNEG) (RegP PT)
--
sRC_RZ :: Src
sRC_RZ = SrcReg msEmpty (RegR RZ)
--
sRC_URZ :: Src
sRC_URZ = SrcReg msEmpty (RegUR URZ)
--
sRC_UPF :: Src
sRC_UPF = SrcReg (msSingleton ModLNEG) (RegUP UPT)
sRC_UPT :: Src
sRC_UPT = SrcReg msEmpty (RegUP UPT)
--
sRC_SR_CTAID_X :: Src
sRC_SR_CTAID_X = SrcReg msEmpty (RegSR SR_CTAID_X)
sRC_SR_TID_X :: Src
sRC_SR_TID_X = SrcReg msEmpty (RegSR SR_TID_X)
--
sRC_I32_0 :: Src
sRC_I32_0 = SrcI32 0
sRC_I32_1 :: Src
sRC_I32_1 = SrcI32 1
sRC_I32_0xF :: Src
sRC_I32_0xF = SrcI32 0xF

srcIntern :: Src -> Src
srcIntern = internLookup sHARED_SRCS

internLookup :: Ord a => DM.Map a a -> a -> a
internLookup m a =
  case a `DM.lookup` m  of
    Just a1 -> a1
    Nothing -> a
--
sHARED_SRCS :: DM.Map Src Src
sHARED_SRCS = DM.fromList $ map (\x -> (x,x)) $
  [
    sRC_PT
  , SrcReg msEmpty (RegP P0)
  , SrcReg msEmpty (RegP P1)
  , SrcReg msEmpty (RegP P2)
  , SrcReg msEmpty (RegP P3)
  --
  , sRC_NPT
  , SrcReg (msSingleton ModLNEG) (RegP P0)
  , SrcReg (msSingleton ModLNEG) (RegP P1)
  , SrcReg (msSingleton ModLNEG) (RegP P2)
  , SrcReg (msSingleton ModLNEG) (RegP P3)
  --
  , sRC_RZ
  , SrcReg msEmpty (RegR R0)
  , SrcReg msEmpty (RegR R1)
  , SrcReg msEmpty (RegR R2)
  , SrcReg msEmpty (RegR R3)
  , SrcReg msEmpty (RegR R4)
  , SrcReg msEmpty (RegR R5)
  , SrcReg msEmpty (RegR R6)
  , SrcReg msEmpty (RegR R7)
  --
  , sRC_URZ
  , SrcReg msEmpty (RegUR UR0)
  , SrcReg msEmpty (RegUR UR1)
  , SrcReg msEmpty (RegUR UR2)
  , SrcReg msEmpty (RegUR UR3)
  --
  , SrcI32 0xFFFFFFFF
  , sRC_I32_0
  , sRC_I32_1
  , SrcI32 0x02
  , SrcI32 0x04
  , SrcI32 0x08
  , SrcI32 0x10
  -- FP32
  , SrcI32 0xFF800000 -- -INF
  , SrcI32 0xFFC00000 -- -QNAN
  , SrcI32 0xC0000000 -- -2
  , SrcI32 0xBF800000 -- -1
  , SrcI32 0xBF000000 -- -0.5
  , SrcI32 0xBE800000 -- -0.25
  -- 0.0 is above
  , SrcI32 0x3E800000 -- 0.25
  , SrcI32 0x3F000000 -- 0.5
  , SrcI32 0x3F800000 -- 1
  , SrcI32 0x40000000 -- 2
  , SrcI32 0x7F800000 -- +INF
  , SrcI32 0x7FC00000 -- +QNAN
  --
  , sRC_SR_TID_X
  , sRC_SR_CTAID_X
  --
  , SrcC0RZX 0
  , SrcC0URZX 0
  --
  , sRC_UPF
  , SrcReg msEmpty (RegUP UP0)
  , SrcReg msEmpty (RegUP UP1)
  , SrcReg (msSingleton ModLNEG) (RegUP UP0)
  , SrcReg (msSingleton ModLNEG) (RegUP UP1)
  , sRC_UPT
  ]

data Imm =
    Imm8  !Word8 -- for LEA, LOP3, ...
  | Imm32 !Word32
  | Imm49 !Int64 -- for branches
  deriving (Show,Eq,Ord)
data Surf =
    SurfImm !Int
  | SurfReg !UR
  deriving (Show,Eq,Ord)
instance Syntax Surf where
  format (SurfImm i) = printf "c[0x%X]" i
  format (SurfReg ur) = "cx[" ++ format ur ++ "]"


-- operand modifiers
data Mod =
  -- prefix modifiers
    ModABS   -- |R12|
  | ModLNEG  -- !R12 (logical negation)
  | ModANEG  -- -R12 (arithmetic negation)
  | ModBNEG  -- ~R12 (bitwise negation)
  -- NOTE: we assume everything after BNEG is a suffix decorator
  --
  -- suffix modifiers
  --
  | ModREU   -- R12.reuse
  -- these suffixes all follow the .reuse flag
  | ModH0_H0 -- HADD2 ... R12.reuse.H0_H0
  | ModH1_H1 -- ...
  | ModROW -- IMMA ... R12.reuse.ROW
  | ModCOL --
  --
  | ModB1 -- R2P.{B1,B2,B3} [77:76]
  | ModB2
  | ModB3
  --
  -- more suffixes, but for LD*/ST* addr operands
  | ModU32   -- [R12.U32]
  | Mod64    -- [R12.64]
  -- LDS/STS
  | ModX4    -- LDS ... [R12.X4]
  | ModX8    -- LDS ... [R12.X8]
  | ModX16   -- LDS ... [R12.X16]
  deriving (Show,Eq,Ord,Enum)

type ModSet = ES.EnumSet Mod
msElem :: Mod -> ModSet -> Bool
msElem = ES.elem
msEmpty :: ModSet
msEmpty = ES.empty
msNull :: ModSet -> Bool
msNull = ES.null
msNonNull :: ModSet -> Bool
msNonNull = not . ES.null
msUnion :: ModSet -> ModSet -> ModSet
msUnion = ES.union
msHasNegation :: ModSet -> Bool
msHasNegation ms = any (`msElem`ms) [ModLNEG,ModANEG,ModBNEG]
msToList :: ModSet -> [Mod]
msToList = ES.toList
msFromList :: [Mod] -> ModSet
msFromList = msIntern . ES.fromList
msSingleton :: Mod -> ModSet
msSingleton = ES.singleton
msIntern :: ModSet -> ModSet
msIntern ms -- for object sharing
  | msNull ms = msEmpty
  | otherwise = ms
mMutuallyExclusive :: Mod -> Mod -> Bool
mMutuallyExclusive m1 m2 = m1 == m2 || check sets
  where check [] = False
        check (ms:mss) = m1 `msElem` ms && m2 `msElem` ms || check mss

        sets :: [ModSet]
        sets = map msFromList
          [
            [ModLNEG,ModANEG,ModBNEG]
          , [ModX4,ModX8,ModX16]
          , [ModH0_H0,ModH1_H1]
          , [ModB1,ModB2,ModB3]
          , [ModROW,ModCOL]
          , [ModU32,Mod64]
          ]
-- .reuse follows |..|; -|R12|.reuse
msDecorate :: ModSet -> String -> String
msDecorate ms body = prefixes ++ abs_body ++ suffixes
  where abs_body = if has ModABS then ("|" ++ body ++ "|") else body
        prefixes = neg ++ comp ++ pneg
          where neg = if has ModANEG then "-" else ""
                comp = if has ModBNEG then "~" else ""
                pneg = if has ModLNEG then "!" else ""
        suffixes = concatMap fmtSfx (msToList ms)
        has = (`msElem`ms)
        fmtSfx m
          | m`msElem`ms =
            if m > ModBNEG then msFormatSuffix m else ""
          | otherwise = ""
instance Syntax Mod where
  format m =
    case m of
      ModABS -> "|..|"
      ModANEG -> "-"
      ModBNEG -> "~"
      ModLNEG -> "!"
      _ -> msFormatSuffix m

msFormatSuffix :: Mod -> String
msFormatSuffix m =
  case m of
    ModREU -> ".reuse"
    _ -> "." ++ drop 3 (show m)


data PR = P0 | P1 | P2 | P3 | P4 | P5 | P6 | PT
  deriving (Show,Eq,Enum,Read,Ord)

data R =
    R0   | R1   | R2   | R3   | R4   | R5   | R6   | R7
  | R8   | R9   | R10  | R11  | R12  | R13  | R14  | R15
  | R16  | R17  | R18  | R19  | R20  | R21  | R22  | R23
  | R24  | R25  | R26  | R27  | R28  | R29  | R30  | R31
  | R32  | R33  | R34  | R35  | R36  | R37  | R38  | R39
  | R40  | R41  | R42  | R43  | R44  | R45  | R46  | R47
  | R48  | R49  | R50  | R51  | R52  | R53  | R54  | R55
  | R56  | R57  | R58  | R59  | R60  | R61  | R62  | R63
  | R64  | R65  | R66  | R67  | R68  | R69  | R70  | R71
  | R72  | R73  | R74  | R75  | R76  | R77  | R78  | R79
  | R80  | R81  | R82  | R83  | R84  | R85  | R86  | R87
  | R88  | R89  | R90  | R91  | R92  | R93  | R94  | R95
  | R96  | R97  | R98  | R99  | R100 | R101 | R102 | R103
  | R104 | R105 | R106 | R107 | R108 | R109 | R110 | R111
  | R112 | R113 | R114 | R115 | R116 | R117 | R118 | R119
  | R120 | R121 | R122 | R123 | R124 | R125 | R126 | R127
  | R128 | R129 | R130 | R131 | R132 | R133 | R134 | R135
  | R136 | R137 | R138 | R139 | R140 | R141 | R142 | R143
  | R144 | R145 | R146 | R147 | R148 | R149 | R150 | R151
  | R152 | R153 | R154 | R155 | R156 | R157 | R158 | R159
  | R160 | R161 | R162 | R163 | R164 | R165 | R166 | R167
  | R168 | R169 | R170 | R171 | R172 | R173 | R174 | R175
  | R176 | R177 | R178 | R179 | R180 | R181 | R182 | R183
  | R184 | R185 | R186 | R187 | R188 | R189 | R190 | R191
  | R192 | R193 | R194 | R195 | R196 | R197 | R198 | R199
  | R200 | R201 | R202 | R203 | R204 | R205 | R206 | R207
  | R208 | R209 | R210 | R211 | R212 | R213 | R214 | R215
  | R216 | R217 | R218 | R219 | R220 | R221 | R222 | R223
  | R224 | R225 | R226 | R227 | R228 | R229 | R230 | R231
  | R232 | R233 | R234 | R235 | R236 | R237 | R238 | R239
  | R240 | R241 | R242 | R243 | R244 | R245 | R246 | R247
  | R248 | R249 | R250 | R251 | R252 | R253 | R254 | RZ
  deriving (Show,Eq,Enum,Read,Ord)

-- used in DEPBAR
data SB =
    SB0
  | SB1
  | SB2
  | SB3
  | SB4
  | SB5
  deriving (Show,Eq,Enum,Read,Ord)


-- These vary by product.  Some of the commented out registers are from old hardware
data SR =
    SR_LANEID
  | SR_CLOCK
  | SR_VIRTCFG
  | SR_VIRTID
  --
  | SR4 | SR5 | SR6 | SR7 | SR8 | SR9 | SR10 | SR11 | SR12 | SR13 | SR14
  --
  | SR_ORDERING_TICKET
  | SR_PRIM_TYPE
  | SR_INVOCATION_ID
  | SR_Y_DIRECTION
  | SR_THREAD_KILL
  | SM_SHADER_TYPE
  | SR_DIRECTCBEWRITEADDRESSLOW -- SR_DIRECTCBEWRITEADDRESSL
  | SR_DIRECTCBEWRITEADDRESSHIGH -- SR_DIRECTCBEWRITEADDRESSH
  | SR_DIRECTCBEWRITEENABLED
  | SR_SW_SCRATCH -- SR_MACHINE_ID_0
  | SR_MACHINE_ID_1
  | SR_MACHINE_ID_2
  | SR_MACHINE_ID_3
  | SR_AFFINITY
  | SR_INVOCATION_INFO
  | SR_WSCALEFACTOR_XY
  | SR_WSCALEFACTOR_Z
  | SR_TID
  | SR_TID_X -- SR_TID.X
  | SR_TID_Y
  | SR_TID_Z
  | SR36
  | SR_CTAID_X -- SR_CTAID.X
  | SR_CTAID_Y
  | SR_CTAID_Z
  | SR_NTID
  | SR_CirQueueIncrMinusOne
  | SR_NLATC
  | SR43
  | SR_SM_SPA_VERSION
  | SR_MULTIPASSSHADERINFO
  | SR_LWINHI
  | SR_SWINHI
  | SR_SWINLO
  | SR_SWINSZ
  | SR_SMEMSZ
  | SR_SMEMBANKS
  | SR_LWINLO
  | SR_LWINSZ
  | SR_LMEMLOSZ
  | SR_LMEMHIOFF
  | SR_EQMASK
  | SR_LTMASK
  | SR_LEMASK
  | SR_GTMASK
  | SR_GEMASK
  | SR_REGALLOC
  | SR_BARRIERALLOC
  | SR63
  | SR_GLOBALERRORSTATUS
  | SR65
  | SR_WARPERRORSTATUS
  | SR_VIRTUALSMID
  | SR_VIRTUALENGINEID
  --
  | SR69 | SR70 | SR71 | SR72 | SR73 | SR74 | SR75 | SR76 | SR77 | SR78 | SR79
  --
  | SR_CLOCKLO
  | SR_CLOCKHI
  | SR_GLOBALTIMERLO
  | SR_GLOBALTIMERHI
  | SR_ESR_PC
  | SR_ESR_PC_HI
  --
  | SR86 | SR87 | SR88 | SR89 | SR90 | SR91 | SR92 | SR93 | SR94 | SR95
  --
  | SR_HWTASKID
  | SR_CIRCULARQUEUEENTRYINDEX -- SR_CIRCULARQUEUEENTRYINDE
  | SR_CIRCULARQUEUEENTRYADDRESSLOW -- SR_CIRCULARQUEUEENTRYADDR
  | SR_CIRCULARQUEUEENTRYADDRESSHIGH -- this was missing in the past
  --
  | SR_PM0 | SR_PM_HI0
  | SR_PM1 | SR_PM_HI1
  | SR_PM2 | SR_PM_HI2
  | SR_PM3 | SR_PM_HI3
  | SR_PM4 | SR_PM_HI4
  | SR_PM5 | SR_PM_HI5
  | SR_PM6 | SR_PM_HI6
  | SR_PM7 | SR_PM_HI7
  --
  | SR_SNAP_PM0 | SR_SNAP_PM_HI0
  | SR_SNAP_PM1 | SR_SNAP_PM_HI1
  | SR_SNAP_PM2 | SR_SNAP_PM_HI2
  | SR_SNAP_PM3 | SR_SNAP_PM_HI3
  | SR_SNAP_PM4 | SR_SNAP_PM_HI4
  | SR_SNAP_PM5 | SR_SNAP_PM_HI5
  | SR_SNAP_PM6 | SR_SNAP_PM_HI6
  | SR_SNAP_PM7 | SR_SNAP_PM_HI7
  --
  | SR_VARIABLE_RATE
  | SR_TTU_TICKET_INFO
  --
  | SR134 | SR135 | SR136 | SR137
  | SR138 | SR139 | SR140 | SR141 | SR142 | SR143 | SR144 | SR145
  | SR146 | SR147 | SR148 | SR149 | SR150 | SR151 | SR152 | SR153
  | SR154 | SR155 | SR156 | SR157 | SR158 | SR159 | SR160 | SR161
  | SR162 | SR163 | SR164 | SR165 | SR166 | SR167 | SR168 | SR169
  | SR170 | SR171 | SR172 | SR173 | SR174 | SR175 | SR176 | SR177
  | SR178 | SR179 | SR180 | SR181 | SR182 | SR183 | SR184 | SR185
  | SR186 | SR187 | SR188 | SR189 | SR190 | SR191 | SR192 | SR193
  | SR194 | SR195 | SR196 | SR197 | SR198 | SR199 | SR200 | SR201
  | SR202 | SR203 | SR204 | SR205 | SR206 | SR207 | SR208 | SR209
  | SR210 | SR211 | SR212 | SR213 | SR214 | SR215 | SR216 | SR217
  | SR218 | SR219 | SR220 | SR221 | SR222 | SR223 | SR224 | SR225
  | SR226 | SR227 | SR228 | SR229 | SR230 | SR231 | SR232 | SR233
  | SR234 | SR235 | SR236 | SR237 | SR238 | SR239 | SR240 | SR241
  | SR242 | SR243 | SR244 | SR245 | SR246 | SR247 | SR248 | SR249
  | SR250 | SR251 | SR252 | SR253 | SR254
  --
  | SRZ
  --
  | SR_CgaCtaId
  deriving (Eq,Show,Ord,Enum)

data UR =
    UR0   | UR1   | UR2   | UR3   | UR4   | UR5   | UR6   | UR7
  | UR8   | UR9   | UR10  | UR11  | UR12  | UR13  | UR14  | UR15
  | UR16  | UR17  | UR18  | UR19  | UR20  | UR21  | UR22  | UR23
  | UR24  | UR25  | UR26  | UR27  | UR28  | UR29  | UR30  | UR31
  | UR32  | UR33  | UR34  | UR35  | UR36  | UR37  | UR38  | UR39
  | UR40  | UR41  | UR42  | UR43  | UR44  | UR45  | UR46  | UR47
  | UR48  | UR49  | UR50  | UR51  | UR52  | UR53  | UR54  | UR55
  | UR56  | UR57  | UR58  | UR59  | UR60  | UR61  | UR62  | URZ
  deriving (Show,Eq,Enum,Read,Ord)

data UP =
    -- Older assemblers used to produce UP7, but I believe this
    -- should have been UPT since it was treated that way logically
    -- in operations (i.e. as an ignored unshown identity value).
    UP0 | UP1 | UP2 | UP3 | UP4 | UP5 | UP6 | UPT
  deriving (Show,Eq,Enum,Read,Ord)

-- Barrier convergence registers (c.f. BSSY)
data BR =
    B0  | B1  | B2  | B3  | B4  | B5  | B6  | B7
  | B8  | B9  | B10 | B11 | B12 | B13 | B14 | B15
  --
  -- c.f. BMOV
  | THREAD_STATE_ENUM_0 -- "THREAD_STATE_ENUM.0"
  | THREAD_STATE_ENUM_1
  | THREAD_STATE_ENUM_2
  | THREAD_STATE_ENUM_3
  | THREAD_STATE_ENUM_4
  | TRAP_RETURN_PC_LO -- "TRAP_RETURN_PC.LO"
  | TRAP_RETURN_PC_HI -- "TRAP_RETURN_PC.HI"
  | TRAP_RETURN_MASK
  | MEXITED
  | MKILL
  | MACTIVE
  | MATEXIT
  | OPT_STACK
  | API_CALL_DEPTH
  | ATEXIT_PC_LO -- "ATEXIT_PC.LO"
  | ATEXIT_PC_HI -- "ATEXIT_PC.HI"
  deriving (Show,Eq,Enum,Read,Ord)

-- used in TEX and TLD as a source operand
data TexOp =
    TexOp1D
  | TexOp2D
  | TexOp3D
  | TexOpCUBE
  | TexOpARRAY_1D
  | TexOpARRAY_2D
  | TexOpINVALID6
  | TexOpARRAY_CUBE
  deriving (Show,Eq,Ord,Enum)

-- this type omits src reuse info as we couple that with the operands
data DepInfo =
  DepInfo {
    diStalls :: !Int
  , diYield :: !Bool
  , diAllocWr :: !(Maybe Int)
  , diAllocRd :: !(Maybe Int)
  , diWaitSet :: !Word32
  } deriving (Show,Eq)
instance Syntax DepInfo where
  format di =
      "{" ++ intercalate "," (filter (not . null) tks) ++ "}"
    where tks =
            waits ++ [
              alloc "R" diAllocRd
            , alloc "W" diAllocWr
            , stalls
            , yield
            ]
          waits = map tryBit [0..7]
            where tryBit  i
                    | testBit (diWaitSet di) i = "^" ++ show (i+1)
                    | otherwise = ""
          stalls = if diStalls di == 0 then "" else ("!" ++ show (diStalls di))
          yield = if diYield di then "Y" else ""

          alloc what f =
            case f di of
              Nothing -> ""
              Just b -> "+" ++ show b ++ "." ++ what
diIntern :: DepInfo -> DepInfo
diIntern di
  | diDefault == di = diDefault   -- {}
  | diStall1 == di = diStall1     -- {!1}
  | diStall2 == di = diStall2     -- {!2}
  | diStall3 == di = diStall3     -- {!3}
  | diStall4Y == di = diStall4Y   -- {!4,Y}
  | diStall10Y == di = diStall10Y -- {!10,Y}
  | diStall12Y == di = diStall12Y -- {!12,Y}
  | otherwise = di
diDefault, diStall1, diStall2, diStall3, diStall4Y, diStall10Y, diStall12Y :: DepInfo
diDefault =
  DepInfo {
    diStalls = 0
  , diYield = False
  , diAllocWr = Nothing
  , diAllocRd = Nothing
  , diWaitSet = 0
  }
diStall1 = diDefault{diStalls = 1}
diStall2 = diDefault{diStalls = 2}
diStall3 = diDefault{diStalls = 3}
diStall4Y = diDefault{diStalls = 4,diYield = True}
diStall10Y = diDefault{diStalls = 10,diYield = True}
diStall12Y = diDefault{diStalls = 12,diYield = True}

instance Codeable PR where
  encode = encodeEnum
  decode = decodeEnum P0 PT
instance Syntax PR where format = show

instance Codeable R where
  encode = encodeEnum
  decode = decodeEnum R0 RZ
instance Syntax R where format = show

instance Syntax SR where
  format sr
    | "SR_TID_" `isPrefixOf` str = "SR_TID." ++ drop (length "SR_TID_") str
    | "SR_CTAID_" `isPrefixOf` str = "SR_CTAID." ++ drop (length "SR_CTAID_") str
    | otherwise = str
    where str = show sr
instance Codeable SR where
  encode = encodeEnum
  decode = decodeEnum SR_LANEID SRZ

instance Codeable UR where
  encode = encodeEnum
  decode = decodeEnum UR0 URZ
instance Syntax UR where format = show

instance Codeable UP where
  encode = encodeEnum
  decode = decodeEnum UP0 UPT
instance Syntax UP where format = show

instance Codeable BR where
  encode = encodeEnum
  decode = decodeEnum B0 ATEXIT_PC_HI
instance Syntax BR where
  format b =
    case b of
      THREAD_STATE_ENUM_0 -> "THREAD_STATE_ENUM.0"
      THREAD_STATE_ENUM_1 -> "THREAD_STATE_ENUM.1"
      THREAD_STATE_ENUM_2 -> "THREAD_STATE_ENUM.2"
      THREAD_STATE_ENUM_3 -> "THREAD_STATE_ENUM.3"
      THREAD_STATE_ENUM_4 -> "THREAD_STATE_ENUM.4"
      TRAP_RETURN_PC_LO -> "TRAP_RETURN_PC.LO"
      TRAP_RETURN_PC_HI -> "TRAP_RETURN_PC.HI"
      ATEXIT_PC_LO -> "ATEXIT_PC.LO"
      ATEXIT_PC_HI -> "ATEXIT_PC.HI"
      _ -> show b

instance Codeable SB where
  encode = encodeEnum
  decode = decodeEnum SB0 SB5
instance Syntax SB where format = show

instance Codeable TexOp where
  encode = encodeEnum
  decode = decodeEnum TexOp1D TexOpARRAY_CUBE
instance Syntax TexOp where
  format = drop (length "TexOp") . show


-- instruction options are the tokens following the mnemonic
type InstOptSet = ES.EnumBitSet Word256 InstOpt
iosToList :: InstOptSet -> [InstOpt]
iosToList = ES.toList
iosFromList :: [InstOpt] -> InstOptSet
iosFromList = iosIntern . ES.fromList
iosElem :: InstOpt -> InstOptSet -> Bool
iosElem = ES.elem
iosIntersect :: InstOptSet -> InstOptSet -> InstOptSet
iosIntersect ios_l = iosIntern . ES.intersect ios_l
iosEmpty :: InstOptSet
iosEmpty = ES.empty
iosNull :: InstOptSet -> Bool
iosNull = ES.null
iosIntern :: InstOptSet -> InstOptSet
iosIntern ios = if iosNull ios then iosEmpty else ios


----------------
-- TODO: refactor
-- data InstOpts =
--   | InstOptsNONE
--   | InstOptsSETP !InstOptsSETP
--   | InstOptsIO !InstOptsIO
--
--   InstOptsOLD
--  deriving ...
--
-- data InstOptsIO =
--   InstOptsIO !DataSize !AddrSize ![IOOpts]


data InstOpt =
    -- ISETP/UISETP
    InstOptF
  | InstOptLT
  | InstOptEQ
  | InstOptLE
  | InstOptGT
  | InstOptNE
  | InstOptGE
  | InstOptT
  -- FSETP/HSETP
  | InstOptEQU
  | InstOptNEU
  | InstOptLTU
  | InstOptLEU
  | InstOptGEU
  | InstOptGTU
  | InstOptNUM
  | InstOptNAN
  | InstOptMIN -- also REDUX
  | InstOptMAX -- also REDUX
  --
  | InstOptEX -- ISETP
  --
  -- REDUX
  | InstOptSUM
  --
  | InstOptAND
  | InstOptOR
  | InstOptXOR
  --
  -- W
  | InstOptW -- SGXT.W
  --
  -- FLO.*
  | InstOpSH
  --
  -- ATOM.*
  | InstOptADD
  | InstOptEXCH
  | InstOptCAS
  | InstOptCAST
  | InstOptARRIVE -- ATOMS.ARRIVE.64
  | InstOptPOPC -- ATOMS.POPC.INC.32
  | InstOptSPIN
  --
  -- B2R.RESULT
  | InstOptRESULT
  | InstOptWARP
  --
  -- BAR.SYNC
  | InstOptARV
  | InstOptDEFER_BLOCKING
  | InstOptRED
  | InstOptSYNC
  | InstOptSYNCALL
  --
  -- BRA.{INC,DEC} (bit [86:85])
  | InstOptINC
  | InstOptDEC
  | InstOptDIV -- sm_90 (BRA.DIV)
  --
  -- rounding modes in FMP, FSETP, etc...
  | InstOptFTZ -- FSETP, others
  | InstOptRZ  -- round to zero
  | InstOptRE  -- round to even
  | InstOptRP  -- round to plus inf
  | InstOptRM  -- round to minus inf
  | InstOptRN  -- round to negative
  --
  | InstOptWIDE -- IMAD
  --
  | InstOptX    -- IADD3.X
  --
  | InstOptSAT  -- I2I.U16.S32.SAT
  --
  | InstOptPAND -- LOP3.LUT.PAND
  --
  | InstOptL -- SHF.L / QSPC.L
  | InstOptR -- SHF.R
  --
  | InstOptHI -- SHF, IADD3, LEA, ...
  --
  -- SHFL
  | InstOptUP
  | InstOptDOWN
  | InstOptIDX
  | InstOptBFLY
  --
  -- PRMT
  | InstOptF4E
  | InstOptB4E
  | InstOptRC8
  | InstOptECL
  | InstOptECR
  | InstOptRC16
  --
  | InstOptREL -- call/ret
  | InstOptABS
  | InstOptNODEC
  | InstOptNOINC
  --
  -- MUFU.*
  | InstOptCOS
  | InstOptSIN
  | InstOptEX2
  | InstOptLG2
  | InstOptRCP
  | InstOptRSQ
  | InstOptRCP64H
  | InstOptRSQ64H
  | InstOptSQRT
  | InstOptTANH
  -- invalid
  --
  | InstOptE   -- LD*/ST*
  | InstOptU8  -- .U8
  | InstOptS8  -- .S8
  | InstOptU16 -- .U16
  | InstOptS16 -- .S16
  | InstOpt32  -- .32  (BMOV)
  | InstOpt64  -- .64
  | InstOpt128 -- .128
  | InstOptS32 -- .S32 (REDUX)
  | InstOptU64 -- .U64 (I2F)
  | InstOptS64 -- .S64 (I2F)
  -- | InstOptU_128 -- .U.128 (LDS) (gone in CUDA 11 even for old SM)
  --
  | InstOptPRIVATE
  --
  | InstOptZD  -- .ZD (on LDG)
  -- scope
  | InstOptCTA
  | InstOptSM
  | InstOptSYS
  | InstOptGPU
  -- memory ordering
  | InstOptCONSTANT -- .CONSTANT
  | InstOptSTRONG -- .STRONG
  | InstOptMMIO -- .MMIO
  -- caching
  | InstOptEF
  | InstOptEL
  | InstOptLU
  | InstOptEU
  | InstOptNA
  -- membar
  | InstOptSC -- MEMBAR.SC.GPU
  | InstOptVC -- MEMBAR.ALL.VC (also in RED)
  --
  -- QSPC
  | InstOptS  -- QSPC.E.S
  | InstOptG  -- QSPC.E.G
  --
  -- SUST.D.BA.3D.U8.CONSTANT.CTA.PRIVATE.TRAP ..
  ---SUST.D.BA.2D.STRONG.SM.TRAP ..
  | InstOptD
  | InstOptBA
  | InstOpt3D
  | InstOpt2D
  --
  -- TEX.*
  | InstOptAOFFI
  | InstOptB
  | InstOptDC
  | InstOptLL
  | InstOptLB
  | InstOptLZ
  | InstOptNDV
  | InstOptNODEP
  | InstOptSCR
  --
  | InstOptB1 -- P2R
  | InstOptB2 -- P2R
  | InstOptB3 -- P2R
  --
  | InstOptLDGSTSBAR -- ARRIVES.*
  | InstOptCLEAR     -- BMOV.32.CLEAR
  | InstOptPQUAD     -- BMOV.32.PQUAD (bit 84)
  --
  | InstOptTRAP      -- BPT.TRAP
  | InstOptINT       -- BPT.INT
  --
  -- CCTL
  | InstOptC         -- CCTLL
  | InstOptU
  | InstOptI
  --
  | InstOptIV
  | InstOptIVALL
  | InstOptIVALLP
  | InstOptPF1
  | InstOptPF2
  | InstOptRS
  | InstOptWB
  | InstOptWBALL
  --
  | InstOpt884       -- e.g. DMMA.884
  | InstOpt8816      -- e.g. DMMA.884
  | InstOpt1684      -- HMMA
  | InstOpt1688      -- HMMA
  | InstOpt16816     -- e.g. HMMA.16816.F32.BF16
  | InstOpt16832     -- e.g. IMMA.16832
  --
  -- NOTE: since order matters in F2F (src vs dst) we fuse
  -- some pairs of options and give them special handling in both
  | InstOptF16_TF32  -- HMMA.1688.F32.TF32
  | InstOptF16_F32   -- F2F.F16.F32
  | InstOptF16_F64   -- F2F.F16.F64 -- not sure if this exists
  | InstOptBF16_F32  -- F2F.BF16.F32
  | InstOptBF16_F64  -- F2F.BF16.F64 -- not sure if this exists
  | InstOptF32_F16   -- F2F.F32.F16
  | InstOptF32_BF16  -- HMMA.16816.F32.BF16
  | InstOptF32_TF32  -- HMMA.1688.F32.TF32
  | InstOptF32_F64   -- F2F.F32.F64
  | InstOptF64_F16   -- F2F.F64.F16
  | InstOptF64_F32   -- F2F.F64.F32
  -- same for I2I
  | InstOptS8_S32    -- I2I.S8.S32
  | InstOptS16_S32   -- I2I.S16.S32
  | InstOptU8S_32    -- I2I.U8.S32
  | InstOptU16_S32   -- I2I.U16.S32
  --
  -- I2F
  | InstOptF64_S16
  | InstOptF64_U16
  --
  -- F2I
  | InstOptS64_F64
  | InstOptU64_F64
  --
  | InstOptU32       -- IMAD
  | InstOptF32       -- HADD.F32
  | InstOptF64       -- FRND.F64.TRUNC
  | InstOptBF        -- HSET2.BF.EQ.AND / FSET.BF.GT.AND
  | InstOptBF16      -- F2FP.BF16.PACK_AB
  | InstOptBF16_V2   -- HFMA2.BF16_V2
  | InstOptF16       -- HMMA.16816.F16
  | InstOptF16x2     -- ATOM....
  --
  -- IDP.*
  | InstOpt4A_U8_U8
  | InstOpt4A_S8_U8
  | InstOpt4A_U8_S8
  | InstOpt4A_S8_S8
  | InstOpt2A_LO_U16_U8
  | InstOpt2A_LO_U16_S8
  | InstOpt2A_LO_S16_U8
  | InstOpt2A_LO_S16_S8
  | InstOpt2A_HI_U16_U8
  | InstOpt2A_HI_U16_S8
  | InstOpt2A_HI_S16_U8
  | InstOpt2A_HI_S16_S8
  --
  | InstOptTRUNC     -- F2I.TRUNC.NTZ
  | InstOptCEIL      -- F2I.U32.CEIL.NTZ
  | InstOptNTZ       -- F2I.TRUNC.NTZ
  | InstOptFMZ       -- FFMA.FMZ.RZ.SAT
  | InstOptFLOOR     -- FRND.FLOOR
  | InstOptMMA       -- HFMA2.MMA
  | InstOptSX32      -- LEA
  | InstOptSH        -- FLO.U32.SH
  | InstOptBYPASS    -- LDGSTS.E.BYPASS.128.ZFILL
  | InstOptZFILL     -- LDGSTS.E.BYPASS.128.ZFILL
  | InstOptLTC128B   -- LDGSTS.E.BYPASS.LTC128B.128.CONSTANT
  | InstOptPACK_AB   -- F2FP.BF16.PACK_AB
  --
  | InstOpt16_M88_4  -- LDSM.16.M88.4
  | InstOpt16_M88_2  -- LDSM.16.M88.2
  | InstOpt16_MT88_4 -- LDSM.16.MT88.4
  -- TODO: maybe others for LDSM
  --
  | InstOpt16_MT88 -- MOVM.16.MT88
  | InstOpt16_M832 -- MOVM.16.M832 (synthetically generated)
  | InstOpt16_M864 -- MOVM.16.M864 (synthetically generated)
  --
  -- MATCH.*
  | InstOptANY
  | InstOptALL
  --
  -- WARPSYNC
  | InstOptEXCLUSIVE
  | InstOptCOLLECTIVE -- (SM90)
  --
  -- FENCE.* (SM90)
  | InstOptVIEW
  --
  -- SYNC.* (SM90)
  | InstOptA0T1
  | InstOptART0
  | InstOptASYNC
  | InstOptCCTL
  | InstOptPHASECHK
  | InstOptTRANS64
  | InstOptTRYWAIT
  --
  -- ARRIVES.* for SM90
  | InstOptTRANSCNT
  | InstOptARVCNT
  deriving (Show,Eq,Ord,Enum)
  -- WARNING: this mustn't exceed 128 or we need to change our EnumSet to use a Word256


instance Syntax InstOpt where
  format InstOptDEFER_BLOCKING = "DEFER_BLOCKING"
  format InstOptPACK_AB = "PACK_AB"
  format InstOptBF16_V2 = "BF16_V2"
  -- normal cases
  format o = underscoresToDots $ drop (length "InstOpt") (show o)
    where underscoresToDots = map (\c -> if c == '_' then '.' else c)

inst_opt_isetp_functions :: InstOptSet
inst_opt_isetp_functions = iosFromList
  [
    InstOptF
  , InstOptLT
  , InstOptEQ
  , InstOptLE
  , InstOptGT
  , InstOptNE
  , InstOptGE
  , InstOptT
  ]
inst_opt_ldst_types :: InstOptSet
inst_opt_ldst_types = iosFromList [InstOptU8,InstOptS8,InstOptU16,InstOptS16,InstOpt64,InstOpt128]
inst_opt_ldst_scope :: InstOptSet
inst_opt_ldst_scope = iosFromList [InstOptCTA, InstOptSM, InstOptSYS, InstOptGPU]
inst_opt_ldst_ordering :: InstOptSet
inst_opt_ldst_ordering = iosFromList [InstOptCONSTANT, InstOptSTRONG, InstOptMMIO]
inst_opt_ldst_caching :: InstOptSet
inst_opt_ldst_caching = iosFromList [InstOptEF, InstOptEL, InstOptLU, InstOptEU, InstOptNA]

all_inst_opts :: [InstOpt]
all_inst_opts = [toEnum 0 ..]
