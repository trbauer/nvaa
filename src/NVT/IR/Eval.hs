module NVT.IR.Eval where

import NVT.IR.LExpr
import NVT.IR.Types
import NVT.IR.Op
import NVT.Diagnostic
import NVT.Loc

import Data.Bits
import Data.Int


evalInst :: LabelIndex -> Inst -> Either Diagnostic Inst
evalInst ldefs i = do
    srcs <- mapM evalSrc (iSrcs i)
    return i{iSrcs = srcs}
  where evalSrc :: Src -> Either Diagnostic Src
        evalSrc src =
          case src of
            SrcImmExpr loc le
              | oIsBranch (iOp i) -> handleS49
              | otherwise -> handleS32
              where handleS49 = handleS 49 (SrcImm . Imm49)

                    handleS32 = handleS 32 (SrcImm . Imm32 . fromIntegral)

                    handleS :: Int -> (Int64 -> Src) -> Either Diagnostic Src
                    handleS bits cons = do
                      val <- evalLExpr ldefs le
                      if val < -2^(bits-1) || val > 2^(bits-1)-1
                          then Left (dCons loc ("label value is too large for imm"++show bits))
                          else return $ cons val
                      return (cons (fromIntegral val))
            _ -> return src


--  let eval :: Unresolved Src
--      eval lix
--        | oIsBranch op = SrcImm . Imm49 . fromIntegral <$> evalLExpr lix e
--        | otherwise = do
--          val <- evalLExpr lix e
--          if val < -2^31 || val > 2^31-1
--            then Left (dCons loc $ show val ++ ": value overflows 32b imm")
--            else return (SrcI32 (fromIntegral val))
--  -- FIXME: for BR we need to use big op
--  return eval


evalLExpr :: LabelIndex -> LExpr -> Either Diagnostic Int64
evalLExpr ldefs = eval
  where eval :: LExpr -> Either Diagnostic Int64
        eval le =
          case le of
            LExprAdd loc ll l2 -> applyBin loc ll l2 (+)
            LExprSub loc ll l2 -> applyBin loc ll l2 (-)
            LExprMul loc ll l2 -> applyBin loc ll l2 (*)
            LExprDiv loc ll l2 -> applyBinDiv loc ll l2 div
            LExprMod loc ll l2 -> applyBinDiv loc ll l2 mod
            LExprNeg _ l -> applyUnr l negate
            LExprCompl _ l -> applyUnr l complement
            LExprImm _ val -> return val
            --
            LExprLo32 _ le -> applyUnr le (.&.0xFFFFFFFF)
            LExprHi32 _ le -> applyUnr le ((.&.0xFFFFFFFF) . (`shiftR`32))
            LExprSRel loc soff le ->
              case eval le of
                Left d -> Left d
                Right val -> return (val - soff)
            LExprFunFDesc _ le ->
              -- FIXME: implement this at some point
              -- I don't know what a "function descriptor"
              eval le
            --
            LExprLabel loc sym ->
              case sym `lookup` ldefs of
                Nothing -> err loc $ sym ++ ": unbound symbol"
                Just val -> return (fromIntegral val)

        applyBin = applyBinG False
        applyBinDiv = applyBinG True

        applyBinG ::
          Bool ->
          Loc ->
          LExpr -> LExpr -> (Int64 -> Int64 -> Int64) ->
          Either Diagnostic Int64
        applyBinG div loc e1 e2 f = do
          v1 <- eval e1
          v2 <- eval e2
          if div && v2 == 0 then err loc "division by 0"
            else return (f v1 v2)

        applyUnr :: LExpr -> (Int64 -> Int64) -> Either Diagnostic Int64
        applyUnr le f = f <$> eval le

        err :: Loc -> String -> Either Diagnostic a
        err loc = Left . dCons loc
