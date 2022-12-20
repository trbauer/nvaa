module NVT.IR.LExpr where

import NVT.IR.Syntax
import NVT.Loc

import Data.Int
import Text.Printf

-- Label/Listing expressions
--
-- EXAMPLE:
-- _Z10testKerneli: 32@lo((_Z10testKerneli + .L_3@srel))
-- ...
--  /*0120*/       MOV              R20, 32@lo((_Z10testKerneli + .L_3@srel)) {!2};  /* 000FE40000000F00`0000000000147802 */
--  /*0130*/       MOV              R21, 32@hi((_Z10testKerneli + .L_3@srel)) {!7,Y};  /* 000FCE0000000F00`0000000000157802 */
--  /*0140*/       CALL.ABS.NOINC   `(__assertfail) {!5,Y};                    /* 000FCA0003C00000`0000000000007943 */
-- .L_3:
-- ....
--
-- 32@lo((_Z10testKerneli + .L_3@srel)) must mean the low 32 bits of the sum of
-- _Z10testKerneli (device function offset) and the section-relative offset of
-- the label .L_3 (0x150 in this example).  The CUDA binary has:
-- Relocation section '.rela.text._Z10testKerneli' at offset 0xe98 contains 2 entries:
--  Offset          Info           Type           Sym. Value    Sym. Name + Addend
-- 000000000130  000a00000039 unrecognized: 39      0000000000000000 _Z10testKerneli + 150
-- 000000000120  000a00000038 unrecognized: 38      0000000000000000 _Z10testKerneli + 150
--
-- .section .rela.text._Z10testKerneli     RELA
-- 288    _Z10testKerneli    R_CUDA_ABS32_LO_32    336 (0x120)
-- 304    _Z10testKerneli    R_CUDA_ABS32_HI_32    336 (0x130)
--
-- EXAMPLE: 32@lo(fun@fdesc(_Z26computeBezierLinePositionsiP10BezierLinei))
-- computeBezierLinePositions(int, BezierLine*, int)
--  /*04F0*/       UMOV             UR4, 32@lo(fun@fdesc(_Z26computeBezierLinePositionsiP10BezierLinei)) {!1};  /* 000FE20000000000`0000000000047882 */
--  ...
--  /*0510*/       UMOV             UR5, 32@hi(fun@fdesc(_Z26computeBezierLinePositionsiP10BezierLinei)) {!1};  /* 000FE20000000000`0000000000057882 */
-- % readelf -r has
-- 00000000000004f0  000000180000003e unrecognized: 3e      0000000000000000 _Z26computeBezierLinePositionsiP10BezierLinei
-- 0000000000000510  000000180000003f unrecognized: 3f      0000000000000000 _Z26computeBezierLinePositionsiP10BezierLinei
-- ..
--   .section .rel.text._Z21computeBezierLinesCDPP10BezierLinei	REL
--   1296    _Z26computeBezierLinePositionsiP10BezierLinei    R_CUDA_FUNC_DESC32_HI_32
--   1264    _Z26computeBezierLinePositionsiP10BezierLinei    R_CUDA_FUNC_DESC32_LO_32
data LExpr =
  ---------------------------------------------------------------------
  -- binary additive expressions
    LExprAdd !Loc !LExpr !LExpr
  | LExprSub !Loc !LExpr !LExpr
  -- binary multiplicative expressions
  | LExprMul !Loc !LExpr !LExpr
  | LExprDiv !Loc !LExpr !LExpr
  | LExprMod !Loc !LExpr !LExpr
  -- unary expressions
  | LExprNeg   !Loc !LExpr -- -E
  | LExprCompl !Loc !LExpr -- ~E
  --
  ---------------------------------------------------------------------
  -- primary expressions
  --
  -- an immediate value
  | LExprImm !Loc !Int64
  --
  -- a symbol
  | LExprLabel !Loc !String -- e.g. ".L_15"
  --
  -- label functions
  | LExprLo32 !Loc !LExpr -- 32@lo(<LExpr>)
  | LExprHi32 !Loc !LExpr -- 32@hi(<LExpr>)
  --
  -- I think this is section relative offset
  -- .L_3@srel would be the offset of .L_3 relative to the section we are in
  | LExprSRel !Loc !Int64 !LExpr -- <LExpr>@srel (section offset is given)
  --
  -- R_CUDA_FUNC_DESC32_HI_32/R_CUDA_FUNC_DESC32_LO_32
  -- usually (always?) nested in a 32@lo(...)
  | LExprFunFDesc !Loc !LExpr -- fun@fdesc(<LExpr>)
  --
  -- expression grouping (our formatter doesn't really need it below)
  | LExprGrp !Loc !LExpr
  deriving (Show,Eq,Ord)

instance Syntax LExpr where
  format le =
      case le of
        LExprAdd _ l r -> format l ++ " + " ++ format r
        LExprSub _ l r -> format l ++ " - " ++ format r
        LExprMul _ l r -> fmtMulOp l ++ "*" ++ fmtMulOp r
        LExprDiv _ l r -> fmtMulOp l ++ "/" ++ fmtMulOp r
        LExprMod _ l r -> fmtMulOp l ++ "%" ++ fmtMulOp r
        LExprNeg _ e -> "-" ++ fmtUnr e
        LExprCompl _ e -> "~" ++ format e
        LExprImm _ i -> printf "0x%08X" i
        LExprLabel _ lbl -> lbl
        LExprLo32 _ e -> "32@lo(" ++ format e ++ ")"
        LExprHi32 _ e -> "32@hi(" ++ format e ++ ")"
        LExprSRel _ i e -> format e ++ "@srel"
        LExprFunFDesc _ e -> "fun@fdesc(" ++ format e ++ ")"
        LExprGrp _ e -> "(" ++ format e ++ ")"
    where fmtMulOp x
            | isLtMul x = ("(" ++ format x ++ ")")
            | otherwise = format x
          fmtUnr x
            | isLtUnr x = ("(" ++ format x ++ ")")
            | otherwise = format x

          isLtUnr le =
            case le of
              LExprAdd _ _ _ -> True
              LExprSub _ _ _ -> True
              LExprMul _ _ _ -> True
              LExprDiv _ _ _ -> True
              LExprMod _ _ _ -> True
              _ -> False
          isLtMul le =
            case le of
              LExprAdd _ _ _ -> True
              LExprSub _ _ _ -> True
              _ -> False


