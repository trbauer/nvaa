module NVT.PTX.PTXTypes where

import NVT.Loc

import Data.Word


data PTXListing =
  PTXListing {
    plVersion :: (Int,Int)      -- .version 7.2
  , plTarget :: !(SMVer,[String]) -- .target sm_86, texmode_independent
  , plAddrSize :: !Int          -- .address_size 32
  , plFunctions :: ![PTXFunction] -- .entry saxpy
  , plProgramScope :: ![PTXProgramScopeVariable]
  , plLocFiles :: ![PTXFile]    -- .file ....
  , pSections :: [PTXSection] -- .section .debug_str {info_string0: .b8 0}
  } deriving Show

data SMVer =
    SM_75 | SM_80 | SM_86
  deriving (Show,Eq,Enum)

data PTXProgramScopeVariable =
  PTXProgramScopeVariable {
    pvLoc :: !Loc
  , pvAlloc :: !PTXAlloc
  , pvInit :: !(Maybe PTXExpr)
  , pvLinkage :: !(Maybe PTXLinkage)
  } deriving Show

-- .func (.param .b32 retval0) saxpy(...)
-- .entry saxpy(...)
data PTXFunction =
  PTXFunction {
    pfLoc :: !Loc
  , pfName :: !String               -- saxpy
  , pfIsEntry :: !Bool              -- saxpy
  , pfReturn :: !(Maybe PTXAlloc)   -- nothing if .entry, but .func's can be void too
  , pfParams :: ![PTXAlloc]
  , pfElems :: ![PTXElem]           -- function body
  , pfLinkage :: !(Maybe PTXLinkage)
  , pfPerfDirectives :: ![PPTXPerf] -- .maxntid
  } deriving Show

data PTXLinkage =
    PTXLinkageExtern
  | PTXLinkageVisible
  | PTXLinkageWeak
  deriving (Eq,Show,Enum)

data PPTXPerf =
    PPTXPerfMaxnreg !Int
  | PPTXPerfMaxntid !Int !Int !Int
  | PPTXPerfReqntid !Int !Int !Int
  | PPTXPerfMinctapersm !Int
  | PPTXPerfMaxctapersm !Int
  | PPTXPerfPragma ![String]
  | PPTXPerfNoreturn
  deriving Show

-- Basic Type	Fundamental Type Specifiers
-- Signed integer	.s8, .s16, .s32, .s64
-- Unsigned integer	.u8, .u16, .u32, .u64
-- Floating-point	.f16, .f16x2, .f32, .f64
-- Bits (untyped)	.b8, .b16, .b32, .b64
-- Predicate	.pred
data PTXType =
    PTXTypeB8  -- .b8
  | PTXTypeB64 -- .b64
  | PTXTypeB16 -- .b16
  | PTXTypeB32 -- .b32
  --
  | PTXTypeS8  -- .s8
  | PTXTypeS16  -- .s16
  | PTXTypeS32 -- .s32
  | PTXTypeS64 -- .s64
  --
  | PTXTypeU8  -- .u8
  | PTXTypeU16  -- .u16
  | PTXTypeU32 -- .u32
  | PTXTypeU64 -- .u64
  --
  | PTXTypeBF16 -- .bf16
  | PTXTypeF16 -- .f16
  | PTXTypeF16x2 -- .f16x2
  | PTXTypeTF32 -- .tf32
  | PTXTypeF32 -- .f32
  | PTXTypeF64 -- .f64
  --
  | PTXTypePRED -- .pred
  | PTXTypeTEXREF -- .texref
  deriving (Show,Eq,Enum)
ptx_types_all :: [PTXType]
ptx_types_all = [toEnum 0 ..]

-- need to express
-- var         // bare variable            (PTXTypeDim False [])
-- var[]       // variable array index     (PTXTypeDim True [])
-- var[C]      // const                    (PTXTypeDim False [C])
-- var[C][D]   // const multidim           (PTXTypeDim False [C,D])
-- var[][C][D] // const multidim + var.    (PTXTypeDim True [C,D])
data PTXTypeDim =
  PTXTypeDim {
    ptType :: !PTXType -- nested type
  , ptHasVarDim :: !Bool -- final dim is var
  , ptConstDims :: ![Int] -- outer dimensions
  } deriving (Show,Eq)

-- A PTX memory allocation includes
-- parameters, return values and local/shared and other allocations.
-- .param .u32 .ptr .global .align 4 saxpy_param_0,
-- .param .f32                       saxpy_param_1,
-- (.param .align 16 .b8 func_retval0[16])  << return value
-- .local .align 4 .b8 	__local_depot3[4]; << local allocation
-- .reg .f32 %f<5>;
-- .reg .b32 %r<14>;
-- .reg .pred %p<4>;
data PTXAlloc =
  PTXAlloc {
    paSpace :: !PTXSpace
  , paAlignment :: !(Maybe Int)
  , paType :: !PTXTypeDim
  , paPointer :: !(Maybe (PTXSpace,Int))  -- e.g. .ptr .global .align 4
  , paName :: !String
  , paReplicate :: !Int -- for .reg %f<13>
  } deriving Show

data PTXSpace =
    PTXSpaceReg
  | PTXSpaceSReg
  | PTXSpaceParam
  | PTXSpaceLocal
  | PTXSpaceShared
  | PTXSpaceConst
  | PTXSpaceGlobal
  | PTXSpaceGeneric
  | PTXSpaceTex
  deriving (Eq,Show,Enum)
ptx_scopes_all :: [PTXSpace]
ptx_scopes_all = [toEnum 0 ..]

-- ld.param.u32  %r3, [saxpy_param_3];
-- mov.b32       %r4, %envreg3;
-- mov.u32       %r5, %ntid.x;
data PTXInst =
  PTXInst {
    piLoc :: !Loc
  , piPred :: !(Maybe (Bool,PTXReg))
  , piOp :: !PTXOp
  , piDst :: !(Maybe PTXOperand)
  , piSrcs :: ![PTXOperand]
  } deriving Show

type PTXOp = String
type PTXReg = String

data PTXOperand =
    PTXOperandReg      !PTXReg
  | PTXOperandSym      !String
  | PTXOperandImm      !Word64
  | PTXOperandAddr     !PTXOperand !Int -- [addr] or [addr+0x10]
  | PTXOperandTexAddr  ![PTXOperand] -- [%rd8, {%f2, %f3}]
  | PTXOperandVec      ![PTXOperand]
  | PTXOperandRval     !PTXOperand -- return value operand in call (...)
  | PTXOperandWithPred !PTXOperand !PTXOperand -- r0|p
  deriving Show

data PTXElem =
    PTXElemAlloc   !Loc !PTXAlloc
  | PTXElemInst    !Loc !PTXInst
  | PTXElemLabel   !Loc !String
  | PTXElemLocDef  !Loc !PTXLoc
  | PTXElemScope   !Loc ![PTXElem]
  | PTXElemPragma  !Loc ![String] -- .pragma "nounroll";
  | PTXElemProto   !Loc !PTXPrototype
  deriving Show

-- prototype_0 : .callprototype (.param .b32 _) _ (.param .b32 _, .param .b32 _) ;
data PTXPrototype =
  PTXPrototype {
    ppLabel :: !String
  , ppReturn :: !(Maybe PTXAlloc) -- nothing if .entry, but .func's can be void too
  , ppParams :: ![PTXAlloc]
  } deriving Show


-- a .loc directive
data PTXLoc =
  PTXLoc {
    plFile :: !Int
  , plLine :: !Int
  , plColumn :: !Int
  , plInlineInfo :: !(String,Int,(Int,Int,Int)) -- function_name label {+ imm}
  } deriving Show

-- .file 0 ....
data PTXFile =
  PTXFile {
    pfIndex :: !Int
  , pfPath :: !FilePath
  , pfTimeStamp  :: !Word64
  , pfFileSize  :: !Int
  } deriving Show

-- .section .debug_str {
-- info_string0:
--  .b8 0
-- }
--
-- .section .debug_pubnames {
--     .b8     0x2b, 0x00, 0x00, 0x00, 0x02, 0x00
--     .b32    .debug_info
--   info_label1:
--     .b32    0x000006b5, 0x00000364, 0x61395a5f, 0x5f736f63
--     .b32    0x6e69616d, 0x63613031, 0x6150736f, 0x736d6172
--     .b8     0x00, 0x00, 0x00, 0x00, 0x00
-- }
-- can have lines like
--    .b32 info_label1+12

data PTXSection =
  PTXSection {
    psName :: !String
  , psSectionLines :: [PTXSectionLine]
  } deriving Show

data PTXSectionLine =
    PTXSectionLineLabel !String
  | PTXSectionLineW8    ![PTXInitValue Word8]
  | PTXSectionLineW16   ![PTXInitValue Word16]
  | PTXSectionLineW32   ![PTXInitValue Word32]
  | PTXSectionLineW64   ![PTXInitValue Word64]
  deriving Show

-- 0x00, 0x00+x, x
data PTXInitValue a = PTXInitValue !String !a
  deriving (Show,Eq)

data PTXExpr =
    PTXExprAdd     !PTXExpr !PTXExpr
  | PTXExprSub     !PTXExpr !PTXExpr
  --
  | PTXExprMul     !PTXExpr !PTXExpr
  | PTXExprDiv     !PTXExpr !PTXExpr
  | PTXExprMod     !PTXExpr !PTXExpr
  --
  | PTXExprNeg     !PTXExpr
  --
  --
  | PTXExprSym     !String
  | PTXExprImm     !Word64
  | PTXExprStruct  ![PTXExpr]
  | PTXExprArray   ![PTXExpr]
  deriving (Show,Eq)
