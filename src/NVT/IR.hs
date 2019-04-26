{-# LANGUAGE PatternSynonyms #-}
module NVT.IR where

import NVT.Bits
import NVT.Encoders.Codable
import NVT.Loc
import NVT.Floats
import qualified NVT.EnumSet as ES

import Data.Bits
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

-- type Op = String
data Op = Op {oMnemonic :: !String}
  deriving (Show,Eq,Ord)
instance Syntax Op where
  format = oMnemonic
oHasDstPred :: Op -> Bool
oHasDstPred (Op op) =
  -- definitely present
  -- e.g.
  --   ISETP.GE.AND P0, PT, R0, c[0x0][0x170], PT {!1} ; // 000FE20003F06270`00005C0000007A0C
  op `elem` ["PLOP3","DSETP","FSETP","ISET","HSETP2"]
oHasOptDstPred :: Op -> Bool
oHasOptDstPred (Op op) =
  -- I THINK THESE ARE SOURCES (there can be two)
  --        IADD3 R10,     R24, R19, R10 {!1} ;   // 000FE20007FFE00A`00000013180A7210
  --  @!P0  IADD3 R12, P1,  R0, R15,  RZ {!5,Y} ; // 000FCA0007F3E0FF`0000000F000C8210

  --   LOP3.LUT     R2, R2,   0x7f800000, RZ, 0xc0, !PT {!4,Y} ;      000fc800078ec0ff`7f80000002027812
  --   LOP3.LUT P1, RZ, R16,     0x20000, RZ, 0xc0, !PT {!12,Y,^4} ;  008fd8000782c0ff`0002000010ff7812
  op `elem` ["LOP3"]

oIsBitwise :: Op -> Bool
oIsBitwise (Op op) = op `elem` ["LOP","PLOP","SHL","SHR","SHF","SHFL"]

oIsBranch :: Op -> Bool
oIsBranch (Op op) =
  op `elem` ["BPT","BRA","BRX","CALL","KILL","NANOTRAP","RET","RTT"]

oIsFP :: Op -> Bool
oIsFP (Op op) =
  op `elem` ["FADD","FFMA","FCHK","FMNMX","FMUL","FSEL","FSET","FSETP"] ||
  op `elem` ["DADD","DFMA","DMUL","DSETP"]
oIsInt :: Op -> Bool
oIsInt (Op op) =
  op `elem` ["I2I","I2F","IABS","IADD3","IDP","IMAD","IMMA","IMNMX","ISETP"]
oIsLD :: Op -> Bool
oIsLD (Op op) =
  op `elem` ["LD","LDC","LDG","LDL","LDS","LDSM"]
oIsST :: Op -> Bool
oIsST (Op op) =
  op `elem` ["ST","STG","STL","STS"]

oIsLDST_L :: Op -> Bool
oIsLDST_L (Op op) =
  op `elem` ["LDL","STL"]
oIsLDST_G :: Op -> Bool
oIsLDST_G (Op op) =
  op `elem` ["LDG","STG","LD","ST"]

oIsSLM :: Op -> Bool
oIsSLM (Op op) = op `elem` ["LDS","LDSM","STS"]

data Inst =
  Inst {
    iLoc :: !Loc
  , iPc :: !PC
  , iPredication :: !Pred
  , iOp :: !Op
  , iOptions :: !InstOptSet -- the mnemonic '.' suffixes
  , iDsts :: ![Dst]
  , iSrcs :: ![Src]
  , iSrcPreds :: ![Pred] -- e.g. IADD3.X has two extra predicate expressions
  , iDepInfo :: !DepInfo
  } deriving (Show,Eq)
instance Syntax Inst where
  format i = fmtInst (defaultImmFormatter (iOp i)) i
-- iLogicallyEqual :: Inst -> Inst -> Bool
-- iLogicallyEqual i0 i1 = i0{iLoc=lNONE} == i1{iLoc=lNONE}


iHasInstOpt :: InstOpt -> Inst -> Bool
iHasInstOpt io = (io`ES.member`) . iOptions
iLacksInstOpt :: InstOpt -> Inst -> Bool
iLacksInstOpt io = not . iHasInstOpt io

fmtInstIr :: Inst -> String
fmtInstIr i =
    "Inst {\n" ++
    fS "iLoc" iLoc ++
    fS "iPc" iPc ++
    fS "iPredication" iPredication ++
    fS "iOp" iOp ++
    fS "iOptions" iOptions ++
    fS "iDsts" iDsts ++
    fS "iSrcs" iSrcs ++
    fS "iSrcPreds" iSrcPreds ++
    fS "iDepInfo" iDepInfo ++
    "}"
  where fS :: Show a => String -> (Inst -> a) -> String
        fS = f show
        f :: (a -> String) -> String -> (Inst -> a) -> String
        f fmt nm prj =
          printf "  %-12s" nm ++ " = " ++ fmt (prj i) ++ "\n"

data Pred =
    PredNONE
    --     neg  reg
  | PredP  !Bool !PR -- regular predicate
  | PredUP !Bool !UP -- uniform predicate (True,UP7) is "UPZ"
                     -- this should act the same as PredNONE
  deriving (Show,Eq)
instance Syntax Pred where
  format pp =
      case pp of
        PredNONE -> ""
        PredP z p -> "@" ++ sign z ++ format p
        PredUP z p -> "@" ++ sign z ++ format p
    where sign z = if z then "!" else ""
-- data PredSign
--   | PredPOS
--   | PredNEG
--   deriving (Show,Eq)

data Reg =
    RegR !R
  | RegP !PR
  | RegB !BR
  | RegUR !UR
  | RegUP !UP
  | RegSR !SR
  deriving (Show,Eq,Ord)
instance Syntax Reg where
  format reg =
    case reg of
      RegR r -> format r
      RegP r -> format r
      RegB r -> format r
      RegUR r -> format r
      RegUP r -> format r
      RegSR r -> format r


data Dst = Dst !Reg deriving (Show,Eq,Ord)
pattern DstR :: R -> Dst
pattern DstR r = Dst (RegR r)
pattern DstRZ = DstR RZ
pattern DstP :: PR -> Dst
pattern DstP p = Dst (RegP p)
pattern DstB :: BR -> Dst
pattern DstB b = Dst (RegB b)
pattern DstUR :: UR -> Dst
pattern DstUR ur = Dst (RegUR ur)
pattern DstUP :: UP -> Dst
pattern DstUP up = Dst (RegUP up)

instance Syntax Dst where
  format (Dst r) = format r



data Src =
    SrcReg !ModSet !Reg
  | SrcCon !ModSet !Surf !Int
  | SrcImm !Imm
  deriving (Show,Eq,Ord)
pattern SrcRZ :: Src
pattern SrcRZ  = SrcReg ES.EnumSetEMPTY (RegR RZ)
pattern SrcURZ = SrcReg ES.EnumSetEMPTY (RegUR URZ)
pattern SrcPT  = SrcReg ES.EnumSetEMPTY (RegP PT)
--
pattern Src_R ms r = SrcReg ms (RegR r)
pattern Src_P ms p = SrcReg ms (RegP p)
pattern Src_UP ms up = SrcReg ms (RegUP up)
pattern Src_B ms b = SrcReg ms (RegB b)
pattern Src_UR ms ur = SrcReg ms (RegUR ur)
pattern SrcI32 i = SrcImm (Imm32 i)
----------------------------------------------------
-- common values
sRC_PT :: Src
sRC_PT = SrcReg msEmpty (RegP PT)
sRC_P0 :: Src
sRC_P0 = SrcReg msEmpty (RegP P0)
sRC_P1 :: Src
sRC_P1 = SrcReg msEmpty (RegP P1)
sRC_NP0 :: Src
sRC_NP0 = SrcReg (msSingleton ModNEG) (RegP P0)
sRC_NP1 :: Src
sRC_NP1 = SrcReg (msSingleton ModNEG) (RegP P1)
sRC_RZ :: Src
sRC_RZ = SrcReg msEmpty (RegR RZ)
sRC_URZ :: Src
sRC_URZ = SrcReg msEmpty (RegUR URZ)
sRC_SR_CTAID_X :: Src
sRC_SR_CTAID_X = SrcReg msEmpty (RegSR SR_CTAID_X)
sRC_SR_TID_X :: Src
sRC_SR_TID_X = SrcReg msEmpty (RegSR SR_TID_X)
sRC_IMM_0 :: Src
sRC_IMM_0 = SrcI32 0
sRC_IMM_1 :: Src
sRC_IMM_1 = SrcI32 1
sRC_C00 :: Src
sRC_C00 = SrcCon msEmpty (SurfImm 0) 0


sHARED_DSTS :: DM.Map Dst Dst
sHARED_DSTS = DM.fromList $ map (\x -> (x,x)) $
  [
    DstP P0
  , DstP P1
  --
  , DstR R0
  , DstR R1
  , DstR R2
  , DstR R3
  , DstR R4
  --
  , DstUR UR0
  , DstUR UR1
  ]

dstIntern :: Dst -> Dst
dstIntern = internLookup sHARED_DSTS
srcIntern :: Src -> Src
srcIntern = internLookup sHARED_SRCS

internLookup :: Ord a => DM.Map a a -> a -> a
internLookup m a =
  case a `DM.lookup` m  of
    Just a1 -> a1
    Nothing -> a
--
-- TODO: use a tree
sHARED_SRCS :: DM.Map Src Src
sHARED_SRCS = DM.fromList $ map (\x -> (x,x)) $
  [
    sRC_PT
  , sRC_P0
  , sRC_P1
  , sRC_NP0
  , sRC_NP1
  --
  , sRC_RZ
  , SrcReg msEmpty (RegR R0)
  , SrcReg msEmpty (RegR R1)
  , SrcReg msEmpty (RegR R2)
  , SrcReg msEmpty (RegR R3)
  , SrcReg msEmpty (RegR R4)
  --
  , sRC_URZ
  , SrcReg msEmpty (RegUR UR0)
  , SrcReg msEmpty (RegUR UR1)
  --
  , SrcI32 0xFFFFFFFF
  , sRC_IMM_0
  , sRC_IMM_1
  , SrcI32 0x2
  , SrcI32 0x4
  , SrcI32 0x8
  , SrcI32 0x10
  --
  , sRC_SR_TID_X
  , sRC_SR_CTAID_X
  ]

data Imm =
    Imm32 !Word32
  | Imm49 !Word64
  deriving (Show,Eq,Ord)
data Surf =
    SurfImm !Int
  | SurfReg !UR
  deriving (Show,Eq,Ord)
instance Syntax Surf where
  format (SurfImm i) = printf "c[0x%X]" i
  format (SurfReg ur) = "cx[" ++ format ur ++ "]"

instance Syntax Src where
  format = formatSrcWithOpts (defaultImmFormatter (Op "NOP"))

type ImmFormatter = Imm -> String
--
defaultImmFormatter :: Op -> ImmFormatter
defaultImmFormatter op imm =
  case imm of
    Imm32 u32
      -- e.g. FADD
      | oIsFP op -> printf "%f" (bitsToFloat u32)
      -- e.g. IMAD
      | oIsInt op && s32 < 0 -> printf "-0x%08X" (negate s32)
      -- everything else
      | otherwise -> printf "0x%08X" u32
      where s32 = fromIntegral u32 :: Int32
    Imm49 u64 -> printf "0x%013X" u64

formatSrcWithOpts :: ImmFormatter -> Src -> String
formatSrcWithOpts fmt_imm src =
  case src of
    SrcReg ms r -> msDecorate ms (format r)
    SrcCon ms six soff -> msDecorate ms (format six ++ printf "[0x%X]" soff)
    SrcImm i -> fmt_imm i


-- operand modifiers
data Mod =
  -- prefix modifiers
    ModABS   -- |R12|
  | ModNEG   -- -R12 or !P1
  | ModCOMP  -- ~R12
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
            [ModX4,ModX8,ModX16]
          , [ModH0_H0,ModH1_H1]
          , [ModROW,ModCOL]
          , [ModU32,Mod64]
          ]
-- .reuse follows |..|; -|R12|.reuse
msDecorate :: ModSet -> String -> String
msDecorate ms body = prefixes ++ abs_body ++ suffixes
  where abs_body = if has ModABS then ("|" ++ body ++ "|") else body
        prefixes = neg ++ comp
          where neg = if has ModNEG then "-" else ""
                comp = if has ModCOMP then "~" else ""
        suffixes = concatMap fmtSfx (msToList ms)
        has = (`msElem`ms)
        fmtSfx m
          | m`msElem`ms =
            if m > ModCOMP then msFormatSuffix m else ""
          | otherwise = ""
instance Syntax Mod where
  format m =
    case m of
      ModABS -> "|..|"
      ModNEG -> "-"
      ModCOMP -> "-"
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
  | SR_DIRECTCBEWRITEADDRESSL
  | SR_DIRECTCBEWRITEADDRESSH
  | SR_DIRECTCBEWRITEENABLED
  | SR_MACHINE_ID_0
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
  | SR_CIRCULARQUEUEENTRYINDE
  | SR_CIRCULARQUEUEENTRYADDR
  | SR_PM0
  | SR_PM_HI0
  | SR_PM1
  | SR_PM_HI1
  | SR_PM2
  | SR_PM_HI2
  | SR_PM3
  | SR_PM_HI3
  | SR_PM4
  | SR_PM_HI4
  | SR_PM5
  | SR_PM_HI5
  | SR_PM6
  | SR_PM_HI6
  | SR_PM7
  | SR_PM_HI7
  | SR_SNAP_PM0
  | SR_SNAP_PM_HI0
  | SR_SNAP_PM1
  | SR_SNAP_PM_HI1
  | SR_SNAP_PM2
  | SR_SNAP_PM_HI2
  | SR_SNAP_PM3
  | SR_SNAP_PM_HI3
  | SR_SNAP_PM4
  | SR_SNAP_PM_HI4
  | SR_SNAP_PM5
  | SR_SNAP_PM_HI5
  | SR_SNAP_PM6
  | SR_SNAP_PM_HI6
  | SR_SNAP_PM7
  | SR_SNAP_PM_HI7
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
    -- there is no UPT (true), but the non-negated use
    -- of UP7 appears to be treated that way in uniform ops
    -- (shows no predication on the instruction)
    UP0 | UP1 | UP2 | UP3 | UP4 | UP5 | UP6 | UP7
  deriving (Show,Eq,Enum,Read,Ord)

data BR =
    B0  | B1  | B2  | B3  | B4  | B5  | B6  | B7
  | B8  | B9  | B10 | B11 | B12 | B13 | B14 | B15
  deriving (Show,Eq,Enum,Read,Ord)

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
  | diDefault == di = diDefault
  | diStall1 == di = diStall1
  | diStall2 == di = diStall2
  | diStall4Y == di = diStall4Y
  | diStall10Y == di = diStall10Y
  | diStall12Y == di = diStall12Y
  | otherwise = di
diDefault, diStall1, diStall2, diStall4Y, diStall10Y, diStall12Y :: DepInfo
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
  decode = decodeEnum UP0 UP7
instance Syntax UP where format = show

instance Codeable BR where
  encode = encodeEnum
  decode = decodeEnum B0 B15
instance Syntax BR where format = show


-- instruction options are the tokens following the mnemonic
type InstOptSet = ES.EnumBitSet Word128 InstOpt
iosToList :: InstOptSet -> [InstOpt]
iosToList = ES.toList
iosFromList :: [InstOpt] -> InstOptSet
iosFromList = ES.fromList
iosElem :: InstOpt -> InstOptSet -> Bool
iosElem = ES.elem
iosIntersect :: InstOptSet -> InstOptSet -> InstOptSet
iosIntersect = ES.intersect
iosEmpty :: InstOptSet
iosEmpty = ES.empty
iosNull :: InstOptSet -> Bool
iosNull = ES.null


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
  -- DSETP
  | InstOptMAX
  --
  | InstOptEX -- ISETP
  --
  | InstOptAND
  | InstOptOR
  | InstOptXOR
  --
  | InstOptFTZ -- FSETP, others
  | InstOptRZ -- round to zero
  | InstOptRE -- round to even
  | InstOptRP -- round to plus inf
  | InstOptRM -- round to minus inf
  --
  | InstOptU32 -- IMAD
  --
  | InstOptWIDE -- IMAD
  --
  | InstOptX -- IADD3.X
  --
  | InstOptL -- SHF.L
  | InstOptR -- SHF.R
  --
  | InstOptHI -- SHF, IADD3, ...
  --
  -- SHFL
  | InstOptUP
  | InstOptDOWN
  | InstOptIDX
  | InstOptBFLY
  --
  | InstOptREL -- call/ret
  | InstOptABS
  | InstOptNODEC
  | InstOptNOINC
  --
  | InstOptRCP -- MUFU.*
  | InstOptLG2
  | InstOptEX2
  | InstOptRSQ
  --
  | InstOptE   -- LD*/ST*
  | InstOptU   -- .U (part of .U.128)
  | InstOptU8  -- .U8
  | InstOptS8  -- .S8
  | InstOptU16 -- .U16
  | InstOptS16 -- .S16
  | InstOpt64  -- .64
  | InstOpt128 -- .128
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
  -- WARNING: this mustn't exceed 64 or we need to change our EnumSet
  deriving (Show,Eq,Ord,Enum)
instance Syntax InstOpt where
  -- format = ('.':) . drop (length "InstOpt") . show
  format = drop (length "InstOpt") . show
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


fmtInst :: ImmFormatter -> Inst -> String
fmtInst fmt_imm i =
    pred ++ op_str ++ " " ++ opnds_str ++ depinfo_str ++ ";"
  where pred = padR 5 (format (iPredication i))
        op_str = padR 12 (format (iOp i) ++ inst_opts)
          where inst_opts =
                  synthetic_tokens ++
                  concatMap (\io -> "." ++ format io) (iosToList (iOptions i))

        -- This is annoying, but for exact matching of nvdisasm, we have to
        -- print helpful hints on IMADs that are used for identity operations.
        -- Personally, I think the SHL is particularly distracting since the
        -- shift value isn't represented explicitly.
        --  * .MOV  .. X            <-  RZ*Y + X
        --                          or   X*1 + RZ
        --                          or   1*X + RZ
        --  * .IADD .. X, Y         <-  X*1 + Y
        --  * .SHL  K, X            <-  X*(2^K) + RZ
        synthetic_tokens :: String
        synthetic_tokens
          | iOp i == Op "IMAD" =
            case iSrcs i of
              (src0:src1:src2:_)
                | isRZ src0 || isRZ src1 -> ".MOV"   -- RZ*X + Y = MOV Y
                | isEqI 1 src1 && isRZ src2 -> ".MOV" -- X*1 + 0 = MOV X
                | isR src0 && isEqI 1 src1 -> ".IADD"
                | isPow2Gt1 src1 && isRZ src2 -> ".SHL"
                where isRZ SrcRZ = True
                      isRZ _ = False
                      isR (Src_R _ _) = True
                      isR _ = False
                      isEqI k (SrcI32 x) = x == k
                      isEqI _ _ = False
                      isPow2Gt1 (SrcI32 imm) =
                        imm > 1 && (imm .&. (imm - 1)) == 0
                      isPow2Gt1 _ = False
              _ -> ""
          -- unless I choose to use the LUT as syntax
          | iOp i == Op "LOP3" || iOp i == Op "PLOP3" = ".LUT"
          | otherwise = ""

        opnds_str :: String
        opnds_str
          | oIsLD (iOp i) = intercalate "," dsts ++ ", " ++ fmtAddrs (iSrcs i)
          | oIsST (iOp i) = st_src_addr ++ ", " ++ st_src_data
          | otherwise = intercalate ", " (dsts ++ srcs ++ ext_pred_srcs)
          where (st_src_addr,st_src_data) =
                  case splitAt 3 (iSrcs i) of
                    (src_addrs,[src_dat]) ->
                      (fmtAddrs src_addrs,formatSrcWithOpts fmt_imm src_dat)
                    _ -> ("?","?")

                -- we attempt to copy nvdisasm here (at least for LDG/LDS);
                -- specifically, we omit default values except when all are default;
                -- then we emit RZ only
                fmtAddrs :: [Src] -> String
                fmtAddrs srcs = "[" ++ intercalate "+" (concatMap fmtSrc srcs) ++ "]"
                  where opIsDefault :: Src -> Bool
                        opIsDefault SrcRZ = True
                        opIsDefault SrcURZ = True
                        opIsDefault (SrcI32 0) = True
                        opIsDefault _ = False

                        all_default = all opIsDefault srcs

                        fmtSrc src =
                          case src of
                            SrcRZ
                              | not (opIsDefault src) || all_default -> [formatSrcWithOpts fmt_imm src]
                            _ -> [formatSrcWithOpts fmt_imm src]

                dsts = map format (iDsts i)
                srcs = map (formatSrcWithOpts fmt_imm) visible_srcs

                visible_srcs
                  | iOp i == Op "IADD3" =
                    case iSrcs i of
                      -- (SrcP False PT:SrcP False PT:sfx) -> sfx
                      -- (SrcP False PT:sfx) -> sfx
                      SrcPT:SrcPT:sfx -> sfx
                      SrcPT:sfx -> sfx
                      sfx -> sfx
                  | otherwise = iSrcs i
                ext_pred_srcs = map (drop 1 . format) (iSrcPreds i) -- drop 1 for the @ format produces

        depinfo_str = if null d then "" else (" " ++ d)
          where d = format (iDepInfo i)
