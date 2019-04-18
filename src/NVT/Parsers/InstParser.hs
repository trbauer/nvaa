module NVT.Parsers.InstParser where

import NVT.Parsers.Parser
import NVT.Bits
import NVT.Diagnostic
import NVT.Floats
import NVT.Loc
import NVT.IR
import NVT.Encoders.Codable

import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.Bits
import Data.Int
import Data.List
import Data.Word
import Debug.Trace
import Text.Parsec((<|>),(<?>))
import qualified Text.Parsec           as P

-- data LblExpr =
--    LblExprDiff !LblExpr !LblExpr -- (.L_18 - micro)
--  | LblExprLit  !Int64
--  deriving Show

type LabelIndex = [(String,PC)]
type Unresolved a = LabelIndex -> Either Diagnostic a

sequenceUnresolved :: [Unresolved a] -> Unresolved [a]
sequenceUnresolved as lbl_ix =  sequence (map ($lbl_ix) as)
-- sequenceUnresolved :: [Unresolved a] -> Unresolved [a]
-- sequenceUnresolved as lbl_ix = resolve as []
--   where resolve [] ras = reverse ras
--         resolve (ua:uas) ras =
--           case ua lbl_ix of
--             Left err -> Left err
--             Right a -> resolve uas (a:ras)


parseInst ::
  PC ->
  FilePath ->
  Int ->
  String ->
  Either Diagnostic (Inst,[Diagnostic])
parseInst pc fp lno syntax =
  case parseUnresolvedInst pc fp lno syntax of
    Left err -> Left err
    Right (ci,ws) ->
      case ci [] of
        Left err -> Left err
        Right i -> Right (i,ws)

parseUnresolvedInst ::
  PC ->
  FilePath ->
  Int ->
  String ->
  Either Diagnostic (Unresolved Inst,[Diagnostic])
parseUnresolvedInst pc = runPI (pInst pc)


parseInstsUnresolved ::
  LabelIndex ->
  PC ->
  FilePath ->
  Int ->
  String ->
  Either Diagnostic ((Unresolved [Inst],LabelIndex),[Diagnostic])
parseInstsUnresolved lbl_ix0 pc fp lno inp = do
  ((uis,lbl_ix),ws) <- runPI (pBlocks pc lbl_ix0) fp lno inp
  return ((sequenceUnresolved uis,lbl_ix),ws)



pBlocks ::
  PC ->
  LabelIndex ->
  PI ([Unresolved Inst],LabelIndex)
pBlocks = pInstOrLabel []
  where pInstOrLabel :: [Unresolved Inst] -> PC -> LabelIndex -> PI ([Unresolved Inst],LabelIndex)
        pInstOrLabel ruis pc lbl_ix = pTryInst <|> pLabel <|> pEof
          where pTryInst = P.try $ do
                  ui <- pInst pc
                  pInstOrLabel (ui:ruis) (pc+16) lbl_ix
                pLabel = pWithLoc $ \loc -> do
                  lbl <- pLabelChars
                  case lbl`lookup`lbl_ix of
                    Just x ->
                      pSemanticError loc "label already defined"
                    Nothing -> do
                      pSymbol ":"
                      pInstOrLabel ruis pc ((lbl,pc):lbl_ix)
                pEof = do
                  P.eof
                  return (reverse ruis,lbl_ix)


runPI ::
  PI a ->
  FilePath ->
  Int ->
  String ->
  Either Diagnostic (a,[Diagnostic])
runPI pa fp lno inp =
    case runPID p1 init_pst fp inp of
      Left err -> Left err
      Right (a,ws) -> Right (a,ws)
  where p1 = do
          sp <- P.getPosition
          P.setPosition (P.setSourceLine sp lno)
          pa


testPI :: Show a => PI a -> String -> IO ()
testPI = testPIF show
testPIU :: Show a => LabelIndex -> PI (Unresolved a) -> String -> IO ()
testPIU lix = testPIF fmt
  where fmt ua =
          case ua lix of
            Left err -> show err
            Right a -> show a
testPIF :: (a -> String) -> PI a -> String -> IO ()
testPIF fmt pa inp =
  case runPI (pa <* P.eof) "<interactive>" 1 inp of
    Left d -> putStrLn $ dFormat d
    Right (a,_) -> putStrLn (fmt a)

type PI a = PID PISt a

init_pst :: PISt
init_pst = PISt ()

data PISt =
  PISt {
    pisLabels :: ()
  -- pisLabels :: [LblExpr]
  } deriving Show

pInst :: PC -> PI (Unresolved Inst)
pInst pc = body
  where body = pWithLoc $ \loc -> do
          pWhiteSpace
          prd <- pPred
          op <- pOp
          opts <- pInstOpts op
          dsts <- pDsts op
          P.try $ pSymbol ","
          complete_srcs <- pSrcs pc op
          dep_info <- pDepInfo
          P.try $ pSymbol ";"
          return $ \lbls -> do
            srcs <- sequence (map ($lbls) complete_srcs)
            return $
              Inst {
                iLoc = loc
              , iPc = pc
              , iPredication = prd
              , iOp = op
              , iOptions = opts
              , iDsts = dsts
              , iSrcs = srcs
              , iDepInfo = dep_info
              }


pPred :: PI Pred
pPred = P.option PredNONE $ do
    pSymbol "@"
    sign <- P.option False $ pSymbol "!" >> return True
    tryP sign <|> tryUP sign
  where tryP sign = PredP sign <$> pSyntax
        tryUP sign = PredUP sign <$> pSyntax


pOp :: PI Op
pOp = do
  op <- pIdentifier
  -- stuff
  return $ Op op


pInstOpts :: Op -> PI [InstOpt]
pInstOpts op
  | is_lop3 = P.try pLop3Code <|> pOptSeq
  | otherwise = pOptSeq
  where is_lop3 = op `elem` [Op "LOP3",Op "ULOP3"]

        pLop3Code :: PI [InstOpt]
        pLop3Code = do
          pSymbol "."
          _ <- pLop3Expr
          return []

        pOptSeq :: PI [InstOpt]
        pOptSeq = concat <$> P.many pToken
          where pToken = do
                  pSymbol "."
                  P.try pIgnore <|> pInstOpt

        pIgnore :: PI [InstOpt]
        pIgnore
          | op == Op "IMAD" = pSymbols ["MOV"] >> return []
          | is_lop3 = pSymbols ["LUT"] >> return []
          | otherwise = fail "nothing to ignore"

        pInstOpt :: PI [InstOpt]
        pInstOpt = do {t<-pSyntax; return [t]}


pLop3Expr :: PI Word8
pLop3Expr = pOrExpr
  where pOrExpr = pXOrExpr `P.chainl1` pOr
          where pOr = pSymbol "|" >> return (.|.)
        pXOrExpr = pAndExpr `P.chainl1` pXOr
          where pXOr = pSymbol "^" >> return xor
        pAndExpr = pNegExpr `P.chainl1` pAnd
          where pAnd = pSymbol "&" >> return (.&.)
        pNegExpr = pNeg <|> pAtomExpr
          where pNeg = do
                  pSymbol "~"
                  complement <$> pAtomExpr
        pAtomExpr = pGrp <|> pAtom <|> pW8
          where pGrp = do {pSymbol "("; e<-pOrExpr; pSymbol ")"; return e}
                pAtom = pOneOf [("s0",0xF0),("s1",0xCC),("s2",0xAA)]
                pW8 = pHexImm <* pWhiteSpace


pDsts :: Op -> PI [Dst]
pDsts op = do
    dst_pred_opt <- pDstPredOpt
    dst <- pDst
    return $ dst_pred_opt ++ [dst]
  where pDstPredOpt
          | oHasDstPred op = do
              dp <- pDstPred
              pSymbol ","
              return [dp]
          | otherwise = return []



pDst :: PI Dst
pDst = pDstReg <|> pDstPred <|> pDstRegB
pDstReg :: PI Dst
pDstReg = DstR <$> pSyntax
pDstRegB :: PI Dst
pDstRegB = DstB <$> pSyntax
pDstPred :: PI Dst
pDstPred = DstP <$> pSyntax


pSrcs :: PC -> Op -> PI [Unresolved Src]
pSrcs pc op = P.sepBy (pSrc pc op) (pSymbol ",")
pSrc :: PC -> Op -> PI (Unresolved Src)
pSrc pc op = P.try pNonLabel <|> pImmLbl
  where pNonLabel :: PI (Unresolved Src)
        pNonLabel = alwaysSucceeds <$> pCases
          where pCases =
                  P.try pImm <|>
                  P.try pConstInd <|>
                  P.try pConstDir <|>
                  pReg <|>
                  pRegSR <|>
                  pRegUR <|>
                  pRegP <|>
                  pRegB

        alwaysSucceeds :: Src -> Unresolved Src
        alwaysSucceeds src _ = Right src

        pReg :: PI Src
        pReg = do
          (r,neg,abs) <- pWithNegAbs pSyntax
          reuse <- P.option False $ P.try (pSymbol ".reuse" >> return True)
          return $ SrcR neg abs reuse r

        pConstDir :: PI Src
        pConstDir = do
          let pCon = do
                pSymbol "c"
                pSymbol "["
                s <- pInt
                pSymbol "]"
                pSymbol "["
                o <- pInt
                pSymbol "]"
                return (s,o)
          ((s,o),neg,abs) <- pWithNegAbs pCon
          return $ SrcC neg abs s o

        pConstInd :: PI Src
        pConstInd = do
          ((u,o),neg,abs) <-
            pWithNegAbs $ do
              pSymbol "cx"
              pSymbol "["
              u <- pSyntax
              pSymbol "]"
              pSymbol "["
              o <- pInt
              pSymbol "]"
              return (u,o)
          return $ SrcCX neg abs u o

        pNegationNumeric :: PI Bool
        pNegationNumeric = P.option False $ (pSymbol "-" <|> pSymbol "~") >> return True
        pNegationLogical :: PI Bool
        pNegationLogical = P.option False (pSymbol "!" >> return True)

        pWithNegAbs :: PI a -> PI (a,Bool,Bool)
        pWithNegAbs pBody = do
          neg <- pNegationNumeric
          let pWithAbs = do
                  pSymbol "|"
                  r <- pBody
                  pSymbol "|"
                  return (r,True)
          (r,abs) <- pWithAbs <|> (pBody >>= \r -> return (r,False))
          return (r,neg,abs)

        pRegSR :: PI Src
        pRegSR = SrcSR <$> (P.try pUnderbarTidCtaid <|> pSyntax)
          where pUnderbarTidCtaid =
                  -- normally we accept these with a dot, but we allow
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

        pRegUR :: PI Src
        pRegUR = SrcUR <$> pNegationNumeric <*> pSyntax

        pRegP :: PI Src
        pRegP = SrcP <$> pNegationLogical <*> pSyntax

        pRegB :: PI Src
        pRegB = SrcB <$> pSyntax

        -- rejects 32@hi... and 32@lo...
        pImm :: PI Src
        pImm = do
            imm <- pVal
            P.notFollowedBy (P.char '@')
            pWhiteSpace
            return $ SrcI imm
          where pVal
                  | oOpIsFP op = pFltImm <|> pIntImm
                  | otherwise = pIntImm

        pImmLbl :: PI (Unresolved Src)
        pImmLbl = do
          e <- pLExpr pc
          return $ \lix -> SrcI <$> evalLExpr lix e


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
            P.try pLo32 <|>
            P.try pHi32 <|>
            P.try pTickExpr <|> -- `(...)
            P.try pLabel <|>
            pGroup <|>
            pLit
          where pLabel = do
                  ident <- pLabelChars
                  when (ident `elem` ["c","cx"]) $
                    P.notFollowedBy (P.char '[')
                  cons <-
                    P.option id $ pWithLoc $ \loc -> do
                      pSymbol "@srel"
                      return (LExprSRel loc)
                  pWhiteSpace
                  return $ cons $ LExprLabel loc ident
                pGroup = do
                  pSymbol "("
                  e <- pBitExpr
                  pSymbol ")"
                  return e
                pLit = LExprImm loc <$> (pImmA <* pWhiteSpace)
                pLo32 = pSymbol "32@lo" >> LExprLo32 loc <$> pPrimExpr
                pHi32 = pSymbol "32@hi" >> LExprHi32 loc <$> pPrimExpr
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

evalLExpr :: LabelIndex -> LExpr -> Either Diagnostic Word64
evalLExpr lix = (fromIntegral <$>) . eval
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
            LExprLo32 _ le -> applyUnr le (.&.0xFFFFFFFF)
            LExprHi32 _ le -> applyUnr le ((.&.0xFFFFFFFF) . (`shiftR`32))
            --
            LExprLabel loc sym ->
              case sym `lookup` lix of
                Nothing -> err loc "unbound symbol"
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


data LExpr =
  -- binary additive
    LExprAdd !Loc !LExpr !LExpr
  | LExprSub !Loc !LExpr !LExpr
  -- binary multiplicative
  | LExprMul !Loc !LExpr !LExpr
  | LExprDiv !Loc !LExpr !LExpr
  | LExprMod !Loc !LExpr !LExpr
  -- unary expressions
  | LExprNeg !Loc !LExpr -- -E
  | LExprCompl !Loc !LExpr -- ~E
  -- primary expressions
  | LExprImm !Loc !Int64
  | LExprLo32 !Loc !LExpr -- 32@lo(...)
  | LExprHi32 !Loc !LExpr -- 32@hi(...)
  | LExprSRel !Loc !LExpr -- label@srel
  | LExprLabel !Loc !String -- e.g. ".L_15"
  deriving Show


-- oHasSrcs :: Op -> Bool
-- oHasSrcs op =

pIntImm :: PI Word64
pIntImm = do
    maybeNegate <- P.option id (pSymbol "-" >> return negate)
    fromIntegral . maybeNegate <$> pImmA

pFltImm :: PI Word64
pFltImm = do
    maybeNegate <- P.option id $ pSymbol "-" >> return (flip complementBit 31)
    maybeNegate <$> (pInf <|> pNonInf)
  where pInf = pSymbol "INF" >> return 0x7FF00000
        pNonInf = fromIntegral . floatToBits . doubleToFloat <$> pFloating


-- {!8,Y,+2.R,+3.W,^4,^1}
pDepInfo :: PI DepInfo
pDepInfo = (pSymbol "{" >> pTokenLoop 0 di0 ) <|> return di0
  where di0 = DepInfo 0 False Nothing Nothing 0
        pTokenLoop :: Int -> DepInfo -> PI DepInfo
        pTokenLoop ix di = pEnd <|> (pMaybeSep >> pToken)
          where pEnd = do pSymbol "}" >> return di
                pMaybeSep = if ix == 0 then return () else pSymbol_ ","
                pToken =
                  pDist <|>
                    pWait <|>
                      P.try (pAlloc diAllocRd 'R' (\b -> di{diAllocRd = Just b})) <|>
                        pAlloc diAllocWr 'W' (\b -> di{diAllocWr = Just b}) <|>
                          pYield

                pDist = do
                  i <- pSymbol "!" >> pOneOf [(show i,i)|i<-[0..15]]
                  pTokenLoop (ix+1) di{diStalls = i}
                pYield = pSymbol "Y" >> pTokenLoop (ix+1) di{diYield = True}
                pWait = do
                  ix <- P.char '^' >> pBarrier
                  pTokenLoop (ix+1) di{diWaitSet = setBit (diWaitSet di) (ix-1)}

                pAlloc f what cons = do
                  P.char '+'
                  b_ix <- pBarrier
                  P.char '.' >> P.char what
                  case f di of
                    Nothing -> pTokenLoop (ix+1) (cons b_ix)
                    Just _ -> fail $ what:"-dependency barrier already set"

                pBarrier = pOneOf [(show i,i)|i<-[1..7]]


-----------------
pSyntax :: (Enum s,Syntax s) => PI s
pSyntax = pOneOf es
  where es = map (\e -> (format e,e)) [toEnum 0 ..]

