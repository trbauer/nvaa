module NVT.Parsers.OperandParsers where

import NVT.Bits
import NVT.Diagnostic
import NVT.Floats
import NVT.IR
import NVT.Parsers.NVParser


import Control.Monad
import Data.Bits
import Data.Int
import Data.Word
import Text.Parsec((<|>),(<?>))
import qualified Text.Parsec           as P


pDsts :: Op -> PI [Dst]
pDsts op = do
    dst_pred_opt <- pDstPredOpt
    dst <- pDst
    return $ dst_pred_opt ++ [dst]
  where pDstPredOpt
          | oHasDstPred op = do
              dp <- pDstP
              pSymbol ","
              return [dp]
          | otherwise = return []


pDst :: PI Dst
pDst = pDstR <|> pDstP <|> pDstUP <|> pDstUR <|> pDstB
pDstR :: PI Dst
pDstR = pLabel "dst reg" $ DstR <$> pSyntax
pDstB :: PI Dst
pDstB = pLabel "dst barrier reg" $ DstB <$> pSyntax
pDstP :: PI Dst
pDstP = pLabel "dst pred reg" $ DstP <$> pSyntax
pDstUP :: PI Dst
pDstUP = pLabel "dst unif pred reg" $ DstUP <$> pSyntax
pDstUR :: PI Dst
pDstUR = pLabel "dst unif reg" $ DstUR <$> pSyntax


resolved :: a -> Unresolved a
resolved = const . Right
resolveds :: [a] -> [Unresolved a]
resolveds = map resolved


-- floating point (fp32 or fp64)
pSrcRCUF :: PI Src
pSrcRCUF = P.try pSrcR <|> P.try pSrcCCX <|> P.try pSrcUR <|> pSrcFlt32

-- pSrcRCUH2 :: PI Src
-- would parse a pair of fp16 and merge into a W32
-- need an FP32 to FP16 conversion routine

-- integral
pSrcRCUI :: PI Src
pSrcRCUI = P.try pSrcR <|> P.try pSrcCCX <|> P.try pSrcUR <|> pSrcInt32


-- TODO: remove
pSrc :: PC -> Op -> PI (Unresolved Src)
pSrc pc = pWithLoc . pSrcAt pc
pSrcAt :: PC -> Op -> Loc -> PI (Unresolved Src)
pSrcAt pc op loc = P.try pNonLabel <|> pSrcLblAt pc op loc
  where pNonLabel :: PI (Unresolved Src)
        pNonLabel = alwaysSucceeds <$> pCases
          where pCases =
                  P.try pImmNonLabel <|>
                  pSrcCCX <|>
                  pSrcR <|>
                  pSrcSR <|>
                  pSrcUR <|>
                  pSrcUP <|>
                  pSrcP <|>
                  pSrcB

        pImmNonLabel
          | oIsBranch op = pSrcI49 op
          | otherwise = pSrcI32 op

        alwaysSucceeds :: Src -> Unresolved Src
        alwaysSucceeds src _ = Right (srcIntern src)

pSrcLbl :: PC -> Op -> PI (Unresolved Src)
pSrcLbl pc = pWithLoc . pSrcLblAt pc
pSrcLblAt :: PC -> Op -> Loc -> PI (Unresolved Src)
pSrcLblAt pc op loc = pLabel "label expression" $ do
  e <- pLExpr pc
  let eval :: Unresolved Src
      eval lix
        | oIsBranch op = SrcImm . Imm49 . fromIntegral <$> evalLExpr lix e
        | otherwise = do
          val <- evalLExpr lix e
          if val < -2^31 || val > 2^31-1
            then Left (dCons loc "value overflows 32b imm")
            else return (SrcI32 (fromIntegral val))
  -- FIXME: for BR we need to use big op
  return eval


-- imm32, but can be a symbol too
pSrcI32OrL32 :: PC -> Op -> PI (Unresolved Src)
pSrcI32OrL32 pc op = pLabel "imm operand" $
  P.try (pSrcLbl pc op) <|> resolved <$> pSrcI32 op


-- LDC wedges src0 into the src1 expression
-- c[0x3][R3+0x8]
-- cx[UR3][R0+0x8]
pXLdcSrcs :: PI Src -> Src -> PI [Src]
pXLdcSrcs pSrcXR xrz = pLabel "const addr" $ pInd <|> pDir
  where pDir = do
          pSymbol "c"
          pSymbol "["
          s <- pSignedInt
          pSymbol "]"
          pSurfOff (SurfImm s)
        --
        pInd = do
          P.try (pSymbol "cx")
          pSymbol "["
          u <- pSyntax
          pSymbol "]"
          pSurfOff (SurfReg u)
        --
        --  ...[R13+0x10]
        --  ...[R13-0x10]
        --  ...[R13+-0x10]
        --  ...[R13]
        --  ...[0x10] -- means RZ+...
        pSurfOff :: Surf -> PI [Src]
        pSurfOff surf = do
          pSymbol "["
          let pOffsetTerm = pIntTerm
              -- handles both [R13] and [R13+...]
              pRegSum = P.try $ do
                r <- pSrcXR
                off <- P.option 0 pOffsetTerm
                return [r, SrcCon msEmpty surf off]
              --
              pImmOnly = do
                off <- pOffsetTerm <|> pInt
                return [xrz, SrcCon msEmpty surf off]
          --
          srcs <- pRegSum <|> pImmOnly
          pSymbol "]"
          return srcs

pSrcR_MMA :: PI Src
pSrcR_MMA = pLabel "reg src" $ pSrcR_WithModSuffixes [ModREU,ModROW,ModCOL]


pSrcCCXH2 :: PI Src
pSrcCCXH2 = pLabel "const src" $ do
  SrcCon ms s o <- pSrcCCX
  ms_sfx <- pAddModRegSuffixesFrom [ModH0_H0,ModH1_H1]
  return $ SrcCon (ms`msUnion`ms_sfx) s o


pSrcRH2 :: PI Src
pSrcRH2 = pLabel "reg src" $ pSrcR_WithModSuffixes [ModREU,ModH0_H0,ModH1_H1]
pSrcURH2 :: PI Src
pSrcURH2 = pLabel "uniform reg src" $ pSrcUR_WithModSuffixes [ModREU,ModH0_H0,ModH1_H1]

pDstRH2 :: PI Dst
pDstRH2 = pLabel "dst reg" $ do
  dst_r <- pSyntax
  ms_sfx <- pAddModRegSuffixesFrom [ModH0_H0,ModH1_H1]
  return $ DstRms ms_sfx dst_r

-- e.g. R10, R10.H0_H0, or c[0][0], c[0][0].H1_H1, or pair of imm16
-- TODO: pair ImmH2
pSrcXH2s :: PI [Src]
pSrcXH2s =
  box <$> P.try pSrcRH2 <|>
    box <$> P.try pSrcCCXH2 <|>
    box <$> P.try pSrcURH2 <|>
    pSrcImmH2

box :: a -> [a]
box a = [a]

pSrcCCX :: PI Src
pSrcCCX = pLabel "const src" $ P.try pConstInd <|> P.try pConstDir
  where pConstDir :: PI Src
        pConstDir = do
          let pCon = do
                pSymbol "c"
                pSymbol "["
                s <- pInt
                pSymbol "]"
                pSymbol "["
                o <- pSignedInt
                pSymbol "]"
                return (SurfImm s,o)
          ((s,o),ms_neg_abs) <- pWithNegAbs pCon
          return $ SrcCon ms_neg_abs s o

        pConstInd :: PI Src
        pConstInd = do
          ((u,o),ms_neg_abs) <-
            pWithNegAbs $ do
              pSymbol "cx"
              pSymbol "["
              u <- pSyntax
              pSymbol "]"
              pSymbol "["
              o <- pSignedInt
              pSymbol "]"
              return (SurfReg u,o)
          return $ SrcCon ms_neg_abs u o


pSignedInt :: PI Int
pSignedInt = do
  sign <- P.option id $ pSymbol "-" >> return negate
  sign <$> pInt


pSrcImmH2 :: PI [Src]
pSrcImmH2 = do
  -- TODO: these are really supposed to be packed into a single operand
  s0 <- pSrcFlt32
  s1 <- pSymbol "," >> pSrcFlt32
  return [s0,s1]

-- rejects 32@hi... and 32@lo...
pSrcI49 :: Op -> PI Src
pSrcI49 op = pLabel "imm49" $ srcIntern <$> pSrcImmNonBranch49 op
pSrcImmNonBranch49 :: Op -> PI Src
pSrcImmNonBranch49 op = do
  imm <- fromIntegral <$> pIntImm64
  P.notFollowedBy (P.char '@')
  pWhiteSpace
  return $ SrcImm (Imm49 imm)

-- cannot be a label
pSrcI8 :: PI Src
pSrcI8 = pLabel "imm8" $ srcIntern <$> pIt
  where pIt = pLexeme $ do
          w8 <- pImmA :: PI Word8 -- will fail gracefully on overflow
          return (SrcImm (Imm32 (fromIntegral w8)))

pSrcI32 :: Op -> PI Src
pSrcI32 op = pLabel "imm32" $ srcIntern <$> pSrcImmNonBranch32 op
pSrcImmNonBranch32 :: Op -> PI Src
pSrcImmNonBranch32 op = do
    imm <- pVal32
    P.notFollowedBy (P.char '@')
    pWhiteSpace
    return $ SrcI32 imm
  where pVal32
          -- TODO: should we convert integer to FP?
          | oIsFP op = pFltImm32 <|> pIntImm32
          | otherwise = pIntImm32

pSrcFlt32 :: PI Src
pSrcFlt32 = pLabel "imm operand" $ SrcI32 <$> pFltImm32

pSrcInt32 :: PI Src
pSrcInt32 = pLabel "imm operand" $ SrcI32 <$> pIntImm32


-- e.g. +0x10 or -0x10 or +-0x10
pIntTerm :: PI Int
pIntTerm =  pPlusX <|> pMinus
  where pOff modf = modf <$> pInt
        pMinus = pSymbol "-" >> pOff negate
        pPlusX = do
          pSymbol "+"
          pMinus <|> pOff id


pNegationBitwise :: PI ModSet
pNegationBitwise =
  pSymbol "-" >> return (msSingleton ModANEG)
pNegationNumeric :: PI ModSet
pNegationNumeric =
  pSymbol "~" >> return (msSingleton ModBNEG)
pNegationLogical :: PI ModSet
pNegationLogical = P.option msEmpty $
  pSymbol "!" >> return (msSingleton ModLNEG)

pNegationBitwiseOrNumericOpt :: PI ModSet
pNegationBitwiseOrNumericOpt = P.option msEmpty $
  pNegationNumeric <|> pNegationBitwise

-- how to make this work with <$>...<*>...<*>...
-- pWithNegAbsC :: PI a -> PI (Bool -> Bool -> PI a)

pWithNegAbs :: PI a -> PI (a,ModSet)
pWithNegAbs pBody = do
  ms_neg <- pNegationBitwiseOrNumericOpt
  let pWithAbs = do
        pSymbol "|"
        r <- pBody
        pSymbol "|"
        return (r,True)
  (r,abs) <- pWithAbs <|> (pBody >>= \r -> return (r,False))
  let char x a = if x then msSingleton a else msEmpty
  let ms = msIntern $ msUnion ms_neg $ char abs ModABS
  return (r,ms)


pAddModRegSuffixesFrom :: [Mod] -> PI ModSet
pAddModRegSuffixesFrom ms = msIntern . msFromList <$> P.many (pOneOf opts)
  where opts = map (\m -> (msFormatSuffix m,m)) ms

pSrcR :: PI Src
pSrcR = pSrcR_WithModSuffixes [ModREU]

pSrcR_WithModSuffixes :: [Mod] -> PI Src
pSrcR_WithModSuffixes ms = pLabel "reg operand" $ do
  (reg,ms_pfx) <- pWithNegAbs (pLabel "reg" pSyntax)
  ms_sfx <- pAddModRegSuffixesFrom ms
  return $ SrcReg (msIntern (ms_pfx`msUnion`ms_sfx)) (RegR reg)

pSrcUR_WithModSuffixes :: [Mod] -> PI Src
pSrcUR_WithModSuffixes ms = pLabel "uniform reg operand" $ do
  (reg,ms_pfx) <- pWithNegAbs (pLabel "uniform reg" pSyntax)
  ms_sfx <- pAddModRegSuffixesFrom ms
  return $ SrcReg (msIntern (ms_pfx`msUnion`ms_sfx)) (RegUR reg)


pSrcSB :: PI Src
pSrcSB = pLabel "sb reg operand" $ SrcReg msEmpty . RegSB <$> pSyntax

pSrcSR :: PI Src
pSrcSR = pLabel "sys reg operand" $ SrcReg msEmpty . RegSR <$> parse
  where parse = P.try pUnderbarTidCtaid <|> pSyntax
        pUnderbarTidCtaid =
          -- we accept these with a dot, but we also allow
          -- the more IR-consistent form here (SR_TID.X)
          --
          -- the regular form pSyntax will replace the _ with a .
          -- to match nvdisasm's output; thus we favor the nvdisasm
          -- version in output and whatnot
          (pSymbol "SR_TID_X" >> return SR_TID_X) <|>
          (pSymbol "SR_TID_Y" >> return SR_TID_Y) <|>
          (pSymbol "SR_TID_Z" >> return SR_TID_Z) <|>
          (pSymbol "SR_CTAID_X" >> return SR_TID_X) <|>
          (pSymbol "SR_CTAID_Y" >> return SR_TID_Y) <|>
          (pSymbol "SR_CTAID_Z" >> return SR_TID_Z)

pSrcUR :: PI Src
pSrcUR = pLabel "uniform src operand" $ do
  (ur,ms) <- pWithNegAbs pSyntax
  return (Src_UR ms ur)

pSrcP :: PI Src
pSrcP = pLabel "predicate src operand" $ do
  ms <- pNegationLogical
  Src_P ms <$> pSyntax
pSrcUP :: PI Src
pSrcUP = pLabel "uniform predicate src operand" $ do
  ms <- pNegationLogical
  Src_UP ms <$> pSyntax


pSrcB :: PI Src
pSrcB = pLabel "convergence barrier reg operand" $ Src_B msEmpty <$> pSyntax


pLExpr :: PC -> PI LExpr
pLExpr pc = pBitExpr
  where pBitExpr = pAddExpr
        pAddExpr = P.chainl1 pMulExpr pAddOp
          where pAddOp = pOp "+" LExprAdd <|> pOp "-" LExprSub
        pMulExpr = P.chainl1 pUnrExpr pMulOp
          where pMulOp = pOp "*" LExprMul <|> pOp "/" LExprDiv <|> pOp "/" LExprMod
        pOp sym cons = pWithLoc $ \loc -> pSymbol sym >> return (cons loc)

        pUnrExpr = pUnOp "-" LExprNeg <|> pUnOp "~" LExprCompl <|> pPrimExpr
          where pUnOp sym cons = pWithLoc $ \loc -> do
                  pSymbol sym
                  e <- pUnrExpr
                  return $ cons loc e
        pPrimExpr = pWithLoc pPrimExprBody
        pPrimExprBody loc =
            pTryFunc "fun@fdesc" LExprFunFDesc <|>
            pTryFunc "32@lo" LExprLo32 <|>
            pTryFunc "32@hi" LExprHi32 <|>
            P.try pTickExpr <|> -- `(...)
            P.try pLabel <|>
            pGroup <|>
            pLit
          where -- e.g. foo or foo@srel
                pLabel :: PI LExpr
                pLabel = do
                  ident <- pLabelChars
                  when (ident `elem` ["c","cx"]) $
                    P.notFollowedBy (P.char '[') -- not const operands (c[... or cx[...)
                  pModify $ \ps -> ps{pisLabelReferences = (ident,pc):pisLabelReferences ps}
                  cons <-
                    P.option id $ pWithLoc $ \loc -> do
                      --
                      pSymbol "@srel"
                      soff <- pSectionOffset
                      return (LExprSRel loc soff)
                  pWhiteSpace
                  return $ cons $ LExprLabel loc ident
                pGroup = do
                  pSymbol "("
                  e <- pBitExpr
                  pSymbol ")"
                  return e
                pLit = LExprImm loc <$> (pImmA <* pWhiteSpace)
                --
                pTryFunc :: String -> (Loc -> LExpr -> LExpr) -> PI LExpr
                pTryFunc sym cons = P.try $ do
                  pSymbol sym
                  pSymbol "("
                  e <- pBitExpr
                  pSymbol ")"
                  return (cons loc e)
                --
                pTickExpr = pSymbol "`" >> pPrimExpr


pLabelChars :: PI String
pLabelChars = pDotLabel <|> pNonDotLabel
  where pDotLabel = do
          pfx <- P.option "" (P.char '.' >> return ".")
          (pfx++) <$> pNonDotLabel

        pNonDotLabel = do
          let pOtherIdentChar = P.oneOf "_$"
          c0 <- P.letter <|> pOtherIdentChar
          sfx <- P.many $ P.alphaNum <|> pOtherIdentChar
          return (c0:sfx)

evalLExpr :: LabelIndex -> LExpr -> Either Diagnostic Int64
evalLExpr lix = eval
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
              case sym `lookup` lix of
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

-- Label expressions
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
  -- an immediate
  | LExprImm !Loc !Int64
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
  deriving Show


pIntImm32 :: PI Word32
pIntImm32 = do
  -- Were we to use Int32, then fromIntegral to Word64 would sign extend
  -- and create a value that won't fit in encoding.
  --
  -- The function negate :: Word32 -> Word32 is still two's-complement
  let pImmS32 = pImmA :: PI Word32
  maybeNegate <- P.option id (pSymbol "-" >> return negate)
  maybeNegate <$> pImmS32
pIntImm64 :: PI Word64
pIntImm64 = do
  let pImmS64 = pImmA :: PI Word64
  maybeNegate <- P.option id (pSymbol "-" >> return negate)
  maybeNegate <$> pImmS64

pFltImm32 :: PI Word32
pFltImm32 = do
    let pNegSign = pSymbol "-" >> return (flip complementBit 31)
        pSignFunc =
          pNegSign <|> (P.option id (pSymbol "+" >> return id))
    neg <- pSignFunc
    neg <$> (pInf <|> pQNan <|> P.try pNonInf <|> pWholeNumberFlt)
  where pInf = pSymbol "INF" >> return 0x7F800000
        -- pNan = pSymbol "NAN" >> return 0x7FC00000
        pQNan = pSymbol "QNAN" >> return 0x7FC00000
        pNonInf = fromIntegral . floatToBits . doubleToFloat <$> pFloating
        pWholeNumberFlt = do
          f32 <- read <$> P.many1 P.digit :: PI Float
          -- do I want a not-followed by?
          pWhiteSpace
          return (floatToBits f32)


pSyntax :: (Enum s,Syntax s) => PI s
pSyntax = pLabel tnm $ pOneOfKeyword tnm es
  where es = map (\e -> (format e,e)) [toEnum 0 ..]
        tnm = formatTypeName (snd (head es))
