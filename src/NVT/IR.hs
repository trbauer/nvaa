module NVT.IR where

import NVT.Loc
import NVT.Encoders.Codable

import Data.Bits
import Data.Int
import Data.List
import Data.Word
import Text.Printf

type PC = Int

-- type Op = String
data Op = Op {oMnemonic :: !String}
  deriving (Show,Eq,Ord)
instance Syntax Op where
  format = oMnemonic

data Inst =
  Inst {
    iLoc :: !Loc
  , iPc :: !PC
  , iPredication :: !Pred
  , iOp :: !Op
  , iOptions :: ![InstOpt] -- the . suffixes
  , iDsts :: ![Dst]
  , iSrcs :: ![Src]
  , iDepInfo :: !DepInfo
  } deriving (Show,Eq)
instance Syntax Inst where
  format = fmtInst

data Pred =
    PredNONE
    --     sign  reg
  | PredP  !Bool !PR -- regular predicate
  | PredUP !Bool !UP -- uniform predicate (True,UP7) is "UPZ" 
                  -- renders as nothing
  deriving (Show,Eq)
instance Syntax Pred where
  format pp =
      case pp of
        PredP z p -> "@" ++ sign z ++ format p
        PredUP z p -> "@" ++ sign z ++ format p
    where sign z = if z then "!" else "" 
-- data PredSign
--   | PredPOS
--   | PredNEG
--   deriving (Show,Eq)

data Dst =
    DstR !R
  | DstP !PR
  | DstB !BR
  deriving (Show,Eq)
instance Syntax Dst where
  format d =
    case d of
      DstR r -> format r
      DstP r -> format r
      DstB r -> format r

data Src =
    --    neg    abs   reuse  
    SrcR  !Bool !Bool  !Bool  !R   -- register
  | SrcC  !Bool !Bool  !Int   !Int -- constant direct
  | SrcCX !Bool !Bool  !UR    !Int -- constant indirect
  | SrcU                      !UR  -- uniform reg
  | SrcP  !Bool  !PR               -- predication
  | SrcB  !BR                      -- barrier register
  | SrcI  !Int64                   -- immediate (f32 is in the low 32 in binary)
  deriving (Show,Eq)
instance Syntax Src where
  format s =
      case s of
        SrcR neg abs reuse reg -> 
            negAbs neg abs (format reg) ++ reuses
          where reuses = maybeS reuse ".reuse"
        SrcC neg abs six soff -> 
          negAbs neg abs ("c[" ++ show six ++ "][" ++ show soff ++ "]")
        SrcCX neg abs sur soff -> 
          negAbs neg abs ("cx[" ++ format sur ++ "][" ++ show soff ++ "]")
        SrcU ur -> format ur
        SrcP neg pr -> maybeS neg "!" ++ format pr
        SrcB br -> format br
        SrcI i -> printf "0x%08X" (fromIntegral i :: Word32)
    where maybeS z s = if z then s else ""
          negAbs neg abs reg = negs ++ abss ++ reg ++ abss
            where negs = maybeS neg "!" 
                  abss = maybeS abs "|"


sNegated :: Src -> Bool
sNegated s = 
  case s of
    SrcR a _ _ _ -> a
    SrcC a _ _ _ -> a
    SrcCX a _ _ _ -> a
    SrcP a _ -> a
    _ -> False

sAbs :: Src -> Bool
sAbs s = 
  case s of
    SrcR _ n _ _ -> n
    SrcC _ n _ _ -> n
    SrcCX _ n _ _ -> n
    SrcP   n _   -> n
    _ -> False

data PR = P0 | P1 | P2 | P3 | P4 | P5 | P6 | PT
  deriving (Show,Eq,Enum,Read)

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
  deriving (Show,Eq,Enum,Read) 

data UR =
    UR0   | UR1   | UR2   | UR3   | UR4   | UR5   | UR6   | UR7
  | UR8   | UR9   | UR10  | UR11  | UR12  | UR13  | UR14  | UR15
  | UR16  | UR17  | UR18  | UR19  | UR20  | UR21  | UR22  | UR23
  | UR24  | UR25  | UR26  | UR27  | UR28  | UR29  | UR30  | UR31
  | UR32  | UR33  | UR34  | UR35  | UR36  | UR37  | UR38  | UR39
  | UR40  | UR41  | UR42  | UR43  | UR44  | UR45  | UR46  | UR47
  | UR48  | UR49  | UR50  | UR51  | UR52  | UR53  | UR54  | UR55
  | UR56  | UR57  | UR58  | UR59  | UR60  | UR61  | UR62  | URZ
  deriving (Show,Eq,Enum,Read) 

data UP =
    -- there is no UPT (true), but the non-negated use
    -- of UP7 appears to be treated that way in uniform ops
    -- (shows no predication on the instruction)
    UP0 | UP1 | UP2 | UP3 | UP4 | UP5 | UP6 | UP7
  deriving (Show,Eq,Enum,Read) 

data BR =
    B0  | B1  | B2  | B3  | B4  | B5  | B6  | B7
  | B8  | B9  | B10 | B11 | B12 | B13 | B14 | B15
  deriving (Show,Eq,Enum,Read) 

-- omits src reuse info as we couple that with 
-- the operands
data DepInfo =
  DepInfo {
    diStalls :: !Int
  , diYield :: !Bool
  , diWaitSet :: !Word32
  , diAllocRd :: !(Maybe Int)
  , diAllocWr :: !(Maybe Int)
  } deriving (Show,Eq) 
instance Syntax DepInfo where
  format di = 
      "{" ++ intercalate "," (filter (not . null) tks) ++ "}"
    where tks = 
            waits ++ [
              alloc "R" diAllocRd
            , alloc "W" diAllocRd
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

instance Codeable PR where
  encode = encodeEnum
  decode = decodeEnum P0 PT
instance Syntax PR where format = show

instance Codeable R where
  encode = encodeEnum
  decode = decodeEnum R0 RZ
instance Syntax R where format = show

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
data InstOpt =
    -- ISETP/UISETP
    InstOptEQ
  | InstOptNE 
  | InstOptLT 
  | InstOptLE
  | InstOptGE
  | InstOptGT
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
  --
  | InstOptFTZ -- FSETP, others
  | InstOptRZ -- round to zero
  | InstOptRE -- round to even
  | InstOptRP -- round to plus inf
  | InstOptRM -- round to minus inf
  --
  | InstOptU32
  -- 
  | InstOptL -- SHF.R
  | InstOptR
  --
  | InstOptHI -- SHF, IADD3, ...
  --
  | InstOptREL -- call/ret
  | InstOptABS
  | InstOptNODEC
  | InstOptNOINC
  --
  | InstOptRCP
  | InstOptLG2
  | InstOptEX2
  | InstOptRSQ
  --
  | InstOptLUT -- LOP3/PLOP3
  deriving (Show,Eq,Ord,Enum)
instance Syntax InstOpt where
  -- format = ('.':) . drop (length "InstOpt") . show
  format = drop (length "InstOpt") . show

all_inst_opts :: [InstOpt]
all_inst_opts = [toEnum 0 ..]


fmtInst :: Inst -> String
fmtInst i = 
    pred ++ opstr ++ optsstr ++ " " ++ opndsstr ++ depinfostr ++ ";"
  where pred = padR 5 (format (iPredication i))
        opstr = padR 5 (format (iOp i))
        optsstr = padR 10 $ concatMap (\io -> "." ++ format io) (iOptions i) 
        opndsstr = intercalate ", " (dsts ++ srcs)
          where dsts = map format (iDsts i)
                srcs = map format (iSrcs i)
        depinfostr = if null d then "" else (" " ++ d)
          where d = format (iDepInfo i)
