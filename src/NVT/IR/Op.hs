module NVT.IR.Op where

import NVT.IR.Syntax


data Op =
    OpARRIVES    -- ARRIVES.LDGSTSBAR.64 [URZ+0x800] {!1};
  | OpATOM       -- ATOM.E.ADD.STRONG.GPU PT, R9, [R4.64+0x4], R9 {!6,+6.W,+1.R};
  | OpATOMG      -- @!P1 ATOMG.E.ADD.STRONG.GPU PT, R4, [R6.64], R23 {!1,+6.W,+1.R};
  | OpATOMS      -- ATOMS.EXCH       RZ, [0x2804], RZ {!1};
  | OpB2R        -- B2R.RESULT       RZ, P0 {!2,+2.W};
  | OpBAR        -- BAR.SYNC         0x0 {!6};
  | OpBMOV       -- BMOV.32.CLEAR    R24, B6 {!4,+1.W};
  | OpBMSK       -- BMSK             R6, R5, R6 {!2};
  | OpBPT        -- BPT.TRAP         0x1 {!5};
  | OpBRA        -- @!P0  BRA        `(.L_1) {!5}; or @!P1  BRA  !P2, `(.L_18) {!5}; (bit 87-90 are PredSrc) or BRA.DIV          ~URZ, `(.L_989) {!6};
  | OpBRX        -- BRX              R10 -0xbd0 {!5}
  | OpBRXU
  | OpBREAK
  | OpBREV
  | OpBSSY       -- BSSY             B0, `(.L_14) {!1};
  | OpBSYNC      -- BSYNC            B1 {!5};
  | OpCALL       -- CALL.ABS.NOINC   `(cudaLaunchDeviceV2)
  | OpCCTL       -- CCTL.IVALL        {!5,Y};
  | OpCS2R
  | OpDADD
  | OpDEPBAR     -- DEPBAR.LE        SB0, 0x0 {!4,Y};
  | OpDFMA
  | OpDMMA
  | OpDMUL
  | OpDSETP
  | OpERRBAR
  | OpEXIT
  | OpF2F
  | OpF2FP
  | OpF2I
  | OpFADD
  | OpFCHK
  | OpFFMA
  | OpFLO
  | OpFMNMX
  | OpFMUL
  | OpFRND
  | OpFSEL
  | OpFSET
  | OpFSETP
  | OpHADD2
  | OpHFMA2
  | OpHMMA
  | OpHMUL2
  | OpHSET2
  | OpHSETP2
  | OpI2F
  | OpI2I
  | OpI2IP
  | OpIABS
  | OpIADD3
  | OpIDP
  | OpIMAD
  | OpIMMA
  | OpIMNMX
  | OpISET
  | OpISETP
  | OpKILL
  | OpLD
  | OpLDC
  | OpLDG
  | OpLDGDEPBAR -- load global dep barrier
  | OpLDGSTS    -- async global to shared mem (load global, store shared)
  | OpLDL
  | OpLDS
  | OpLDSM
  | OpLEA
  | OpLEPC -- load effective PC
  | OpLOP3
  | OpMATCH
  | OpMEMBAR
  | OpMOV
  | OpMOVM
  | OpMUFU
  | OpNANOSLEEP
  | OpNANOTRAP -- sm_75
  | OpNOP
  | OpP2R
  | OpPLOP3
  | OpPOPC
  | OpPRMT
  | OpQSPC
  | OpR2P
  | OpR2UR
  | OpRED
  | OpREDUX
  | OpRET
  | OpRTT
  | OpS2R
  | OpS2UR
  | OpSEL
  | OpSGXT
  | OpSHL
  | OpSHR
  | OpSHF
  | OpSHFL
  | OpST
  | OpSTG
  | OpSTL
  | OpSTS
  | OpSUST
  | OpTEX
  | OpTLD
  | OpUFLO
  | OpUIADD3
  | OpUIMAD
  | OpUISETP
  | OpULDC
  | OpULEA
  | OpULOP3
  | OpUMOV
  | OpUPLOP3
  | OpUPOPC
  | OpUPRMT
  | OpUSEL
  | OpUSGXT
  | OpUSHF
  | OpVOTE
  | OpVOTEU
  | OpWARPSYNC
  | OpYIELD
  deriving (Show,Eq,Ord,Read,Enum)

all_ops :: [Op]
all_ops = [toEnum 0..]

oMnemonic :: Op -> String
oMnemonic = drop 2 . show

-- type Op = String
-- data Op = Op {oMnemonic :: !String}
--  deriving (Show,Eq,Ord)
instance Syntax Op where
  format = oMnemonic


oHasDstPred :: Op -> Bool
oHasDstPred op =
  -- definitely present
  -- e.g.
  --   ISETP.GE.AND P0, PT, R0, c[0x0][0x170], PT {!1} ; // 000FE20003F06270`00005C0000007A0C
  -- op `elem` ["PLOP3","DSETP","FSETP","ISET","HSETP2"]
  op `elem` [OpPLOP3,OpDSETP,OpFSETP,OpISET,OpHSETP2]
oHasOptDstPred :: Op -> Bool
oHasOptDstPred op =
  -- I THINK THESE ARE SOURCES (there can be two)
  --        IADD3 R10,     R24, R19, R10 {!1} ;   // 000FE20007FFE00A`00000013180A7210
  --  @!P0  IADD3 R12, P1,  R0, R15,  RZ {!5,Y} ; // 000FCA0007F3E0FF`0000000F000C8210

  --   LOP3.LUT     R2, R2,   0x7f800000, RZ, 0xc0, !PT {!4,Y} ;      000fc800078ec0ff`7f80000002027812
  --   LOP3.LUT P1, RZ, R16,     0x20000, RZ, 0xc0, !PT {!12,Y,^4} ;  008fd8000782c0ff`0002000010ff7812
  op `elem` [OpLOP3]

oIsBitwise :: Op -> Bool
oIsBitwise = (`elem` [OpLOP3,OpPLOP3,OpSHL,OpSHR,OpSHF,OpSHFL])

oIsBranch :: Op -> Bool
oIsBranch = (`elem`[OpBPT,OpBRA,OpBRX,OpCALL,OpKILL,OpNANOTRAP,OpRET,OpRTT])

oIsU :: Op -> Bool
oIsU op =
  case op of
    OpUFLO -> True
    OpUIADD3 -> True
    OpUIMAD -> True
    OpUISETP -> True
    OpULDC -> True
    OpULEA -> True
    OpULOP3 -> True
    OpUMOV -> True
    OpUPLOP3 -> True
    OpUPOPC -> True
    OpUPRMT -> True
    OpUSEL -> True
    OpUSGXT -> True
    OpUSHF -> True
    _ -> False

oIsD :: Op -> Bool
oIsD = (`elem`[OpDADD,OpDFMA,OpDMUL,OpDSETP])
oIsF :: Op -> Bool
oIsF = (`elem`[OpF2F,OpF2FP,OpF2I,OpFADD,OpFFMA,OpFCHK,OpFMNMX,
             OpFMUL,OpFSEL,OpFSET,OpFSETP])
oIsH :: Op -> Bool
oIsH = (`elem`[OpHADD2,OpHFMA2,OpHMMA,OpHMUL2,OpHSET2,OpHSETP2])
oIsI :: Op -> Bool
oIsI = (`elem`[OpI2I,OpI2F,OpIABS,OpIADD3,OpIDP,OpIMAD,OpIMMA,OpIMNMX,OpISETP])

oIsFP :: Op -> Bool
oIsFP op = any ($op) [oIsF,oIsH,oIsD]

oIsSetP :: Op -> Bool
oIsSetP op = op `elem` [OpISETP,OpHSETP2,OpFSETP,OpDSETP]


oIsLD :: Op -> Bool
oIsLD = (`elem`[OpLD,OpLDC,OpLDG,OpLDL,OpLDS,OpLDSM])
oIsST :: Op -> Bool
oIsST = (`elem`[OpST,OpSTG,OpSTL,OpSTS])
oIsAT :: Op -> Bool
oIsAT = (`elem`[OpATOM,OpATOMG,OpATOMS])

oIsLDST_L :: Op -> Bool
oIsLDST_L = (`elem` [OpLDL,OpSTL])
oIsLDST_G :: Op -> Bool
oIsLDST_G = (`elem` [OpLDG,OpSTG,OpLD,OpST])

oIsSLM :: Op -> Bool
oIsSLM = (`elem` [OpLDS,OpLDSM,OpSTS,OpATOMS])
