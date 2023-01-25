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
pSrcRCUF op =
  P.try pSrcR <|> P.try (pSrcCCX op) <|> P.try pSrcUR <|> pSrcFlt32 op

-- pSrcRCUH2 :: PI Src
-- would parse a pair of fp16 and merge into a W32
-- need an FP32 to FP16 conversion routine

-- integral
pSrcRCUI :: PC -> Op -> PI Src
pSrcRCUI pc op =
    P.try pSrcR <|> P.try (pSrcCCX op) <|>
      P.try pSrcUR <|> P.try (maybeRemap <$> pSrcLbl pc op) <|> pSrcInt32
  where maybeRemap :: Src -> Src
        maybeRemap src =
          -- we try labels before immediates since
          -- 32@lo(...) parses wrong otherwise
          -- ^^ end of SrcImm operand
          --
          -- Favor simple immediates for complex expressions such as
          --   -0x1 ==> imm instead of label
          -- everything else goes to label
          --
          case src of
            SrcImmExpr _ (LExprImm _ i)
              | i <= 2^31 - 1 -> SrcI32 (fromIntegral i)
            SrcImmExpr _ (LExprNeg _ (LExprImm _ i))
              | i <= 2^31 -> SrcI32 (fromIntegral (negate i))
            _ -> src

pSrcLbl :: PC -> Op -> PI Src
pSrcLbl pc = pWithLoc . pSrcLblAt pc
pSrcLblAt :: PC -> Op -> Loc -> PI Src
pSrcLblAt pc op at =
  pLabel "label expression" $ SrcImmExpr at <$> pLExpr pc



-- imm32, but can be a symbol too
pSrc_I32OrL32 :: PC -> Op -> PI Src
pSrc_I32OrL32 pc op = pLabel "imm operand" $
  P.try (pSrcLbl pc op) <|> pSrc_I32 op


-- LDC/ULDC allows register offsets
-- c[0x3][R3+0x8]
-- cx[UR3][R0+0x8]
-- c[0x0][UR3+0x0]
pXLdcSrcs :: PI Reg -> Reg -> PI Src
pXLdcSrcs pSrcXR xrz = pLabel "const addr" $
    pInd <|> P.try pDir <|> pDirShort
  where pDirShort = do
          P.char 'c'
          s <- pInt
          pSurfOff (SurfImm s)
        pDir = do
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
        -- For LDC
        --  ...[R13+0x10]
        --  ...[R13-0x10]
        --  ...[R13+-0x10]
        --  ...[R13]
        --  ...[0x10] -- means RZ+...
        --
        -- For ULDC
        --  ...[0x10] -- means URZ
        --  ...[UR4+0x10]
        pSurfOff :: Surf -> PI Src
        pSurfOff surf = do
          pSymbol "["
          let pOffsetTerm = pIntTerm
              -- handles both [R13] and [R13+...]
              pRegSum = P.try $ do
                r <- pSrcXR
                off <- P.option 0 pOffsetTerm
                return $ SrcCon msEmpty surf r off
              --
              pImmOnly = do
                off <- pOffsetTerm <|> pInt
                return $ SrcCon msEmpty surf xrz off
          --
          src <- pRegSum <|> pImmOnly
          pSymbol "]"
          return src

pSrcR_MMA :: PI Src
pSrcR_MMA = pLabel "reg src" $ pSrcR_WithModSuffixes [ModREU,ModROW,ModCOL]


pSrcCCXH2 :: Op -> PI Src
pSrcCCXH2 op = pLabel "const src" $ do
  SrcCon ms s r o <- pSrcCCX op
  ms_sfx <- pAddModRegSuffixesFrom [ModH0_H0,ModH1_H1]
  return $ SrcCon (ms`msUnion`ms_sfx) s r o


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
pSrcXH2 op = do
  P.try pSrcRH2 <|> P.try (pSrcCCXH2 op) <|> P.try pSrcURH2 <|> pSrcImmH2 op

pSrcCCX :: Op -> PI Src
pSrcCCX o = pLabel "const src" $ P.try pConstInd <|> P.try pConstDir
  where pConstDir :: PI Src
        pConstDir = do
          let pConSurf = do
                P.try (pSymbol "c0" >> return 0) <|>
                  P.try (pSymbol "c1" >> return 1) <|>
                  P.try (pSymbol "c2" >> return 2) <|>
                  P.try (pSymbol "c3" >> return 3) <|>
                  do {pSymbol "c"; pSymbol "["; pInt <* pSymbol "]"}

          let pCon = do
                s <- pConSurf
                pSymbol "["
                o <- pSignedInt
                pSymbol "]"
                return (SurfImm s,o)
          ((s,o),ms_neg_abs) <- pWithNegAbs pCon
          return $ SrcCon ms_neg_abs s xrz o


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
          return $ SrcCon ms_neg_abs u xrz o

        xrz :: Reg
        xrz
          | oIsU o = RegUR URZ
          | otherwise = RegR RZ

pSignedInt :: PI Int
pSignedInt = do
  sign <- P.option id $ pSymbol "-" >> return negate
  sign <$> pInt

-- TODO: these are really supposed to be packed into a single operand
-- can parse: imm16, imm16
pSrcImmH2 :: Op -> PI Src
pSrcImmH2 op = pNvSyn <|> pOurSyn
  where pNvSyn = do
          imm16x2 <- pFltImm32 op
          let h_hi = fromIntegral (imm16x2 `shiftR` 16)
          let h_lo = fromIntegral (imm16x2 .&. 0xFFFF)
          return (SrcImmH2 h_hi h_lo)
        pOurSyn = do
          pSymbol "("
          s <- pNvSyn
          pSymbol ")"
          return s

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
  where pIt = pLexeme $ SrcI8 <$> pImmA -- will fail gracefully on overflow

pSrc_I32 :: Op -> PI Src
pSrc_I32 op = pLabel "imm32" $ srcIntern <$> pSrcImmNonBranch32 op

pSrcImmNonBranch32 :: Op -> PI Src
pSrcImmNonBranch32 op = do
    imm <- pVal32
    P.notFollowedBy (P.char '@')
    pWhiteSpace
    return imm
  where pVal32
          | oIsFP op = SrcI32 <$> (pFltImm32 op <|> pIntImm32)
          | otherwise = SrcI32 <$> pIntImm32


pSrcFlt32 :: Op -> PI Src
pSrcFlt32 op = pLabel "imm operand" $ SrcI32 <$> pFltImm32 op

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
  where opts = concatMap expandMod ms
        expandMod m
          -- allow my shorthand .reu suffix
          | m == ModREU = [(msFormatSuffix m,m),(".reu",m)]
          | otherwise = [(msFormatSuffix m,m)]


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
          where pUnOp sym cons = pWithLoc $ \at -> do
                  pSymbol sym
                  e <- pUnrExpr
                  return $ cons at e
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
                  return (LExprGrp loc e)
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
  | is_fp16x2 = pF16x2
  | otherwise = pOp
  where pF16x2 = do
          h_hi <- pOp16
          pSymbol ","
          h_lo <- pOp16
          return ((h_hi`shiftL`16) .|. h_lo)

        pOp16 = pWithLoc $ \at -> do
          imm16 <- pOp
          unless (imm16 <= 0xFFFF) $
            pSemanticError at "imm16 too large"
          return $ imm16

        pOp = do
          let sign_bit = if is_fp16x2 then 15 else 31 -- fp64 is shifted by now
          let pNegSign = pSymbol "-" >> return (flip complementBit sign_bit)
          signFunc <- pNegSign <|> (P.option id (pSymbol "+" >> return id))
          signFunc <$> (pInf <|> pQNan <|> pNonInf)

        pInf = pSymbol "INF" >> return inf
        pQNan = pSymbol "QNAN" >> return qnan

        pNonInf :: PI Word32
        pNonInf
          | is_fp16x2 = pWithLoc $ \loc -> do
            f32 <- parseF32
            let w32 = floatToBits f32
            let f16 = floatBitsToHalfBits RoundE w32
            when (halfBitsToFloatBits f16 /= w32) $
              pWarning loc "precision loss"
            return $ fromIntegral f16
          | is_fp64 = pWithLoc $ \loc -> do
            f64b <- doubleToBits <$> pWholeOrDecimal64
            when ((f64b .&. 0xFFFFFFFF) /= 0) $
              pSemanticError loc "fp64 imm not-representible imm32"
            return $ fromIntegral (f64b `shiftR` 32)
          | otherwise = floatToBits <$> parseF32
          where parseF32 :: PI Float
                parseF32 = pWithLoc $ \loc -> do
                  f64 <- pWholeOrDecimal64
                  let f32 = doubleToFloat f64
                  when (floatToDouble f32 /= f64) $
                    pWarning loc "precision loss"
                  return f32


        pWholeOrDecimal64 :: PI Double
        pWholeOrDecimal64 = P.try pFloating <|> pWholeNumberFlt

        is_fp64 :: Bool
        is_fp64 = oIsD op

        is_fp16x2 :: Bool
        is_fp16x2 = oIsH2 op

        qnan :: Word32
        qnan
          | is_fp16x2 = ((f16`shiftL`16) .|. f16)
          | is_fp64 = fromIntegral ((f64_EXP_MASK .|. f64_QNAN_BIT) `shiftR` 32)
          | otherwise = 0x7FC00000
          where f16 = fromIntegral (f16_EXP_MASK .|. f16_QNAN_BIT) :: Word32

        inf :: Word32
        inf
          | is_fp16x2 = ((f16`shiftL`16) .|. f16)
          | is_fp64 = fromIntegral (f64_EXP_MASK `shiftR` 32)
          | otherwise = 0x7F800000
          where f16 = fromIntegral f16_EXP_MASK :: Word32

        pWholeNumberFlt :: PI Double
        pWholeNumberFlt = do
          f64 <- read <$> P.many1 P.digit :: PI Double
          P.notFollowedBy (P.try (P.char '.')) -- do I want a not-followed by?
          pWhiteSpace
          return f64

pSyntax :: (Enum s,Syntax s) => PI s
pSyntax = pLabel tnm $ pOneOfKeyword tnm es
  where es = map (\e -> (format e,e)) [toEnum 0 ..]
        tnm = formatTypeName (snd (head es))
