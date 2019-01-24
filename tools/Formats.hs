module Formats where

import Field
import Word128

format_LDG ::
format_LDG =
  [
  ----------------------------------------------------------------------------
  -- usually 9 bits but can be longer
  -- sometimes the same mnemonics
  -- some of the higher bits in the opcode
  {- [8:0] -}  fOPCODE # always
  {- [11:9] -} , fREGFILE
  --
  {- [15:12] -} , fEXECPRED
  {- [23:16] -} , fDSTREG
  {- [31:24] -} , fSRCREG
  ----------------------------------------------------------------------------
  {- [39:32] -} , fLDST_UREGSRC
  {- [49:40] -} , fLDST_IMMOFF_S24 -- 24b offset
  ----------------------------------------------------------------------------
  {- [66:64] -} , fLDST_SRCPRED
  {- [67] -} , fLDST_SRCPREDSIGN
  {- [69:68] -} , fl "Unknown" 68 2 ["",".LTC64B",".LTC128B"] -- .INVALID3
  {- [71:70] -} , fReserved 70 2
  {- [72] -}, fl "AddressSize" 72 1 ["",".E"]
  {- [75:73] -}, fLDST_DATATYPE
  {- [76] -} , fl "Private" 76 1 ["",".PRIVATE"]
  -- for LDS this controls default .X4, .X8, and .X16
  -- {- [78:77] -} , fLDST_REGEXT -- default, .X4, .X8, .X16
  {- [78:77] -} , fLDST_COHERENCY
  {- [80:79] -} , fLD_SEMANTICS
  {- [83:81] -} , fLDST_DSTPRED
  {- [86:84] -} , fLDST_CACHING
  {- [87] -} , fl "ZD" 87 1 ["",".ZD"] -- implies predicate register must be used
  --
  {- [89:88] -} , fReserved 88 2
  -- bits [91:90] adds a .U32 or .64 (10b,11b) on the reg with [11:9] == 100b
  -- and enables the UR reg [91] for LDS
  {- [91:90] -} , fREGFILE_EXT 90 2
  {- [95:92] -} , fReserved 92 4
  ----------------------------------------------------------------------------
  {- [104:96] -} , fReserved 96 9
  {- [125:105] -} , fSCHEDULING
  {- [127:126] -} , fReserved 126 2
  ]
format_STG ::
format_STG =
  [
  {- [8:0] -}  fOPCODE # always
  {- [11:9] -} , fREGFILE
  --
  {- [15:12] -} , fEXECPRED
  {- [23:16] -} , fDSTREG
  {- [31:24] -} , fSRCREG
  ----------------------------------------------------------------------------
  {- [39:32] -} , fLDST_UREGSRC
  {- [49:40] -} , fLDST_IMMOFF_S24 -- 24b offset
  ----------------------------------------------------------------------------
  {- [71:64] -} , fReserved 64 8
  {- [72] -} , fl "AddressSize" 72 1 ["",".E"]
  {- [75:73] -}, fLDST_DATATYPE
  {- [76] -} , fl "Private" 76 1 ["",".PRIVATE"]
  {- [78:77] -} , fLDST_COHERENCY
  {- [80:79] -} , fST_SEMANTICS
  {- [83:81] -} , fReserved 81 3
  {- [86:84] -} , fLDST_CACHING
  {- [89:87] -} , fReserved 87 3
  -- bit [91:90] adds a .U32 or .64 on addr reg (10b,11b) on the reg with [11:9] == 100b
  {- [91:90] -} , fREGFILE_EXT 90 2
  {- [95:92] -} , fReserved 92 4
  ----------------------------------------------------------------------------
  {- [104:96] -} , fReserved 96 9
  {- [125:105] -} , fSCHEDULING
  {- [127:126] -} , fReserved 126 2
  ]


fLD_SEMANTICS :: Field
fLD_SEMANTICS = fl "Source" 79 2 [".CONSTANT","",".STRONG",".MMIO"]

fST_SEMANTICS :: Field
fST_SEMANTICS = fl "Source" 79 2 ["???",      "",".STRONG",".MMIO"]

fEXECPRED :: Field
fEXECPRED = f "ExecPred" 12 4 fmt
  where fmt _ v
          | v == 0x7 = ""
          | v == 0xF = "@!PT"
          | otherwise = "@" ++ sign ++ "P" ++ show v
          where sign = if v > 0x7 then "!" else ""
fDSTREG :: Field
fDSTREG = f "DstReg" 16 8 $ _ v -> if v == 255 then "RZ" else ("R" ++ show v)
fSRCREG :: Field
fSRCREG = f "SrcReg" 24 8 $ _ v -> if v == 255 then "RZ" else ("R" ++ show v)
--
fLDST_UREGSRC :: Field
fLDST_UREGSRC = f 32 8 "SrcUReg" fmt
  where fmt _ 255 = "URZ"
        fmt _ ur = "UR" ++ show ur
fLDST_IMMOFF_S24 :: Field
fLDST_IMMOFF_S24 = f 40 24 "ImmOff" fmt
  where fmt _ v
          | testBit 23 v = "-" ++ printf "0x%06X" v
          | otherwise = printf "0x%06X" v
-- exists ZD or non-zero
fLDST_SRCPRED :: Field -- weirdly in reverse order 1->P6,...7->P0
fLDST_SRCPRED = f "SrcPredicate" 64 3 $ \_ v -> if v == 0 then "" else ("P" ++ show (7-v))
fLDST_SRCPREDSIGN :: Field -- weirdly in reverse order 1->P6,...7->P0
fLDST_SRCPREDSIGN = f "SrcPredicateSign" 67 1 $ \_ v -> if v == 0 then "" else "!"
fLDST_DATATYPE :: Field
fLDST_DATATYPE = fl "DataType" 73 3 [".U8",".S8",".U16",".S16","",".64",".128",".U.128"]
fLDST_COHERENCY :: Field
fLDST_COHERENCY = fl "Coherency" 77 2 [".CTA",".SM",".GPU",".SYS"]
fLDST_DSTPRED :: Field -- weirdly in reverse order 1->P6,...7->P0
fLDST_DSTPRED = f "DstPredicate" 81 3 $ \_ v -> if v == 7 then "" else ("P" ++ show v)
fLDST_CACHING :: Field
fLDST_CACHING = fl "Access" 84 3 [".EF","",".EL",".LU",".EU",".NA"] -- .INVALID6, .INVLAID7
fSCHEDULING :: Field
fSCHEDULING = f "Dependencies" 105 21 $ \_ v -> printf "TODO: 0x%X" v

-- often bit 91 can be considered an extra bit here
-- [11:9] [91]
-- POPC and IADD3
--   001    0 reg
--   100    0 imm
--   101    0 const
--   101    1 const-unif index
--   110    1 ureg
-- LDG
--   001    0 [imm]
--   100    1 [ureg+imm]
--
fREGFILE :: Field
fREGFILE = fl "RegFile" 9 3 fmt
  where fmt _ v =
          case v of
            1 -> "REG"
            4 -> "IMM"
            5 -> "CON"
            _ -> "???"




-- POPC
formatUNARY_ALU :: Format
formatUNARY_ALU =
  [
  ----------------------------------------------------------------------------
  -- usually 9 bits but can be longer
  -- sometimes the same mnemonics
  -- some of the higher bits in the opcode
  {- [8:0] -}  fOPCODE # always
  {- [11:9] -} fREGFILE # always
  --
  {- [15:12] -} , fEXECPRED
  {- [23:16] -} , fDSTREG
  {- [31:24] -} , fSRCREG
  ----------------------------------------------------------------------------
  --
  ----------------------------------------------------------------------------
  ---
  ----------------------------------------------------------------------------
  {- [104:96] -} , fReserved 96 9
  {- [125:105] -} , fSCHEDULING
  {- [127:126] -} , fReserved 126 2
  ]



format_FADD ::
format_FADD =
    [
    {- [8:0] -}  fOPCODE # always
    {- [11:9] -} , fREGFILE -- Src1.RegFile {1->REG,2->IMM,3->CONST,all others illegal}
    --
    {- [15:12] -} , fEXECPRED
    {- [23:16] -} , fDST_REG
    {- [31:24] -} , fSRC0_REG
    ----------------------------------------------------------------------------
    {- [39:32] -} , fSRC1_REG  # src1IsReg
    {- [63:40] -} , fReserved  # src1IsReg

    {- [39:32] -} , fReserved 32 8        # src1IsCon .&. not cIndirectSurface
    --
    {- [37:32] -} , fSRC1_CSRF_REG 32 6   # src1IsCon .&.     cIndirectSurface -- cx[UR###][...]
    {- [39:38] -} , fReserved 38 2        # src1IsCon .&.     cIndirectSurface
    --
    {- [53:40] -} , fSRC1_COFF 40 14      # src1IsCon -- c[..][THIS/4]
    {- [58:54] -} , fSRC1_CSRF 54 5       # src1IsCon -- c[THIS][..]
    {- [61:59] -} , fReserved 59 3        # src1IsCon
    --
    {- [63:62] -} , fSRC1_MODS # src1IsReg .|. src1IsCon
    --
    --
    {- [63:32] -} , fSRC1_IMM32 # src1IsImm
    ----------------------------------------------------------------------------
    {- [71:64] -} , fReserved 64 8
    {- [73:72] -} , fSRC0_MODS
    {- [76:74] -} , fReserved 74 3
    {- [77] -}    , fSATURATE
    {- [79:78] -} , fROUNDMODE -- 0:default (:re?), 1:rm, 2:rp, 3:rz
    {- [80] -}    , fDENORM -- denorm or FTZ
    {- [92:81] -} , fReserved 81 10
    {- [91] -}    , fIS_CINDREG  -- use fSRC1REG as as uniform register
    {- [95:92] -} , fReserved 92 4
    ----------------------------------------------------------------------------
    {- [104:96] -} , fReserved 96 9
    {- [125:105] -} , fSCHEDULING -- 122 and 124 are Src0.Reuse and Src1.Reuse
    {- [127:126] -} , fReserved 126 2
    ]
  where cIndirectSurface = isSet fIS_CSRF_INDIRECT
        src1IsCon = fREGFILE == 3
        src1IsImm = fREGFILE == 2
        src1IsReg = fREGFILE == 1

format_FFMA ::
format_FFMA =
    [
    {- [8:0] -}  fOPCODE # always
    {- [11:9] -} , fREGFILE3 -- Src1.RegFile {1->RRR,2->RRI,3->RRC,4->RIR,5->RCR,all others illegal}
    --
    {- [15:12] -} , fEXECPRED
    {- [23:16] -} , fDST_REG
    {- [31:24] -} , fSRC0_REG
    ----------------------------------------------------------------------------
    -- ternary formats have a "fixed" operand (always register)
    -- and a "variable" operand that can be reg, imm, or constant
    -- [11:9] specifies which source
    --
    -- variable operand can float to src1 or src2 based on [11:9]
    {- [39:32] -} , fSRCX_REG             # isVarR
    {- [63:40] -} , fReserved 40 23       # isVarR
    --
    {- [39:32] -} , fReserved 32 8        # isVarC .&. not cIndirectSurface
    --
    {- [37:32] -} , fSRCX_CINDREG         # isVarC .&.     cIndirectSurface -- cx[UR###][...]
    {- [39:38] -} , fReserved 38 2        # isVarC .&.     cIndirectSurface
    --
    {- [53:40] -} , fSRCX_COFF 40 14      # isVarC                           -- c[..][THIS/4]
    {- [58:54] -} , fSRCX_CSRF 54 5       # isVarC .&. not cIndirectSurface -- c[THIS][..]
    {- [61:59] -} , fReserved 59 3        # isVarC
    --
    {- [63:62] -} , fSRCX_MODS            # isVarC .|. isVarR
    --
    {- [63:32] -} , fSRCX_IMM32           # isVarI
    ----------------------------------------------------------------------------
    {- [71:64] -} , fSRCR_REG -- fixed source
    {- [73:72] -} , fSRC0_MODS
    {- [75:74] -} , fSRCR_MODS
    {- [76] -}    , fFMZ  -- not sure what this is
    {- [77] -}    , fSATURATE
    {- [79:78] -} , fROUNDMODE -- 0:default (:re?), 1:rm, 2:rp, 3:rz
    {- [80] -}    , fDENORM -- denorm or FTZ
    {- [92:81] -} , fReserved 81 10
    {- [91] -}    , fIS_CINDREG  -- use fSRCX as as uniform register
    {- [95:92] -} , fReserved 92 4
    ----------------------------------------------------------------------------
    {- [104:96] -} , fReserved 96 9
    {- [125:105] -} , fSCHEDULING -- 122,123,124 are Src{0,1,2}.Reuse
    {- [127:126] -} , fReserved 126 2
    ]
  where cIndirectSurface = isSet fIS_CSRF_INDIRECT
        src1IsCon = fREGFILE == 3
        src1IsImm = fREGFILE == 2
        src1IsReg = fREGFILE == 1

