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


-- floating point (fp32 or fp64)
pSrcRCUF :: Op -> PI Src
pSrcRCUF op = P.try pSrcR <|> P.try pSrcCCX <|> P.try pSrcUR <|> pSrcFlt32 op

-- pSrcRCUH2 :: PI Src
-- would parse a pair of fp16 and merge into a W32
-- need an FP32 to FP16 conversion routine

-- integral
pSrcRCUI :: PI Src
pSrcRCUI = P.try pSrcR <|> P.try pSrcCCX <|> P.try pSrcUR <|> pSrcInt32

pSrcLbl :: PC -> Op -> PI Src
pSrcLbl pc = pWithLoc . pSrcLblAt pc
pSrcLblAt :: PC -> Op -> Loc -> PI Src
pSrcLblAt pc op loc = pLabel "label expression" $ SrcImmExpr loc <$> pLExpr pc


-- imm32, but can be a symbol too
pSrc_I32OrL32 :: PC -> Op -> PI Src
pSrc_I32OrL32 pc op = pLabel "imm operand" $
  P.try (pSrc_I32 op) <|> pSrcLbl pc op


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
pSrcXH2 :: Op -> PI Src
pSrcXH2 op =
  P.try pSrcRH2 <|> P.try pSrcCCXH2 <|> P.try pSrcURH2 <|> pSrcImmH2 op

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

-- TODO: these are really supposed to be packed into a single operand
-- can parse: imm16, imm16
pSrcImmH2 :: Op -> PI Src
pSrcImmH2 = pSrcFlt32

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

pSrc_I32 :: Op -> PI Src
pSrc_I32 op = pLabel "imm32" $ srcIntern <$> pSrcImmNonBranch32 op
pSrcImmNonBranch32 :: Op -> PI Src
pSrcImmNonBranch32 op = do
    imm <- pVal32
    P.notFollowedBy (P.char '@')
    pWhiteSpace
    return $ Src_I32 imm
  where pVal32
          -- TODO: should we convert integer to FP?
          | oIsFP op = pFltImm32 op <|> pIntImm32
          | otherwise = pIntImm32

pSrcFlt32 :: Op -> PI Src
pSrcFlt32 op = pLabel "imm operand" $ Src_I32 <$> pFltImm32 op

pSrcInt32 :: PI Src
pSrcInt32 = pLabel "imm operand" $ Src_I32 <$> pIntImm32


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

pFltImm32 :: Op -> PI Word32
pFltImm32 op
  | is_fp16 = pF16
  | otherwise = pOp
  where pF16 = do
          h1 <- pOp
          pSymbol ","
          h2 <- pOp
          return ((h1`shiftL`16) .|. h2)

        pOp = do
          let sign_bit = if is_fp16 then 15 else 31 -- fp64 is shifted by now
          let pNegSign = pSymbol "-" >> return (flip complementBit sign_bit)
          signFunc <- pNegSign <|> (P.option id (pSymbol "+" >> return id))
          signFunc <$> (pInf <|> pQNan <|> pNonInf)

        pInf = pSymbol "INF" >> return inf
        pQNan = pSymbol "QNAN" >> return qnan

        pNonInf :: PI Word32
        pNonInf
          | is_fp16 = pWithLoc $ \loc -> do
            f32 <- parseF32
            let f16 = floatToHalfRaw f32
            when (halfToFloatRaw f16 /= f32) $
              pWarning loc "precision loss"
            return $ fromIntegral f16

          | is_fp64 = pWithLoc $ \loc -> do
            f64b <- doubleToBits <$> pWholeOrDecimal
            when ((f64b .&. 0xFFFFFFFF) /= 0) $
              pSemanticError loc "fp64 imm not-representible imm32"
            return $ fromIntegral (f64b `shiftR` 32)

          | otherwise = floatToBits <$> parseF32
          where parseF32 :: PI Float
                parseF32 = pWithLoc $ \loc -> do
                  f64 <- pWholeOrDecimal
                  let f32 = doubleToFloat f64
                  when (floatToDouble f32 /= f64) $
                    pWarning loc "precision loss"
                  return f32


        pWholeOrDecimal :: PI Double
        pWholeOrDecimal = P.try pFloating <|> pWholeNumberFlt


        is_fp64 = oIsD op
        is_fp16 = oIsH2 op

        qnan :: Word32
        qnan
          | is_fp16 = ((f16`shiftL`16) .|. f16)
          | is_fp64 = fromIntegral ((f64_exp_mask .|. f64_qnan_bit) `shiftR` 32)
          | otherwise = 0x7FC00000
          where f16 = fromIntegral (f16_exp_mask .|. f16_qnan_bit) :: Word32

        inf :: Word32
        inf
          | is_fp16 = ((f16`shiftL`16) .|. f16)
          | is_fp64 = fromIntegral (f64_exp_mask `shiftR` 32)
          | otherwise = 0x7F800000
          where f16 = fromIntegral f16_exp_mask :: Word32

        pWholeNumberFlt = do
          f64 <- read <$> P.many1 P.digit :: PI Double
          -- do I want a not-followed by?
          pWhiteSpace
          return f64

pSyntax :: (Enum s,Syntax s) => PI s
pSyntax = pLabel tnm $ pOneOfKeyword tnm es
  where es = map (\e -> (format e,e)) [toEnum 0 ..]
        tnm = formatTypeName (snd (head es))
