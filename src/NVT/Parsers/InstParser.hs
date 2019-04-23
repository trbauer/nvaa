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


type LabelIndex = [(String,PC)]
type Unresolved a = LabelIndex -> Either Diagnostic a

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
parseInstsUnresolved lbl_ix0 pc fp lno inp =
  runPI (pBlocks pc lbl_ix0) fp lno inp


-------------------------------------------------------------------------------
sequenceUnresolved :: [Unresolved a] -> Unresolved [a]
sequenceUnresolved as lbl_ix =  sequence (map ($lbl_ix) as)
-- sequenceUnresolved :: [Unresolved a] -> Unresolved [a]
-- sequenceUnresolved as lbl_ix = resolve as []
--   where resolve [] ras = reverse ras
--         resolve (ua:uas) ras =
--           case ua lbl_ix of
--             Left err -> Left err
--             Right a -> resolve uas (a:ras)

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

testBlocks :: String -> IO ()
testBlocks inp = testPIF fmt (pBlocks 0 []) inp
  where fmt :: (Unresolved [Inst],LabelIndex) -> String
        fmt (uis,_) =
          case uis [] of
            Left err -> dFormat err
            Right is -> concatMap (\i -> show i ++ "\n") is

type PI a = PID PISt a

init_pst :: PISt
init_pst = PISt ()

data PISt =
  PISt {
    pisLabels :: ()
  -- pisLabels :: [LblExpr]
  } deriving Show

-------------------------------------------------------------------------------
pBlocks ::
  PC ->
  LabelIndex ->
  PI (Unresolved [Inst],LabelIndex)
pBlocks pc0 li0 = seqResult <$> pInstOrLabel [] pc0 li0
  where seqResult (uis,li) = (sequenceUnresolved uis,li)

        pInstOrLabel :: [Unresolved Inst] -> PC -> LabelIndex -> PI ([Unresolved Inst],LabelIndex)
        pInstOrLabel ruis pc lbl_ix =
            P.try pLabel <|> pInstNoFail <|> pEof
          where pInstNoFail = do
                  ui <- P.try (pInst pc)
                  pInstOrLabel (ui:ruis) (pc+16) lbl_ix
                pLabel = pWithLoc $ \loc -> do
                  lbl <- pLabelChars
                  pSymbol ":"
                  case lbl`lookup`lbl_ix of
                    Just x ->
                      pSemanticError loc "label already defined"
                    Nothing -> do
                      pInstOrLabel ruis pc ((lbl,pc):lbl_ix)
                pEof = do
                  P.eof
                  return (reverse ruis,lbl_ix)


pInst :: PC -> PI (Unresolved Inst)
pInst pc = pWhiteSpace >> body
  where body = pWithLoc $ \loc -> do
          prd <- P.option PredNONE pPred
          op <- pOp
          case op of
            Op "IADD3" -> do
              opts <- pInstOpts op
              dsts <- pDsts op
              let dft_pred = SrcP False PT
              (p0,p1) <-
                P.option (dft_pred,dft_pred) $ P.try $ do
                  p0 <- pSymbol "," >> pSrcP
                  p1 <- P.option dft_pred (P.try $ pSymbol "," >> pSrcP)
                  return (p0,p1)
              pIntTernary loc prd op opts dsts (resolveds [p0, p1])
            Op "IMAD" -> do
              opts <- pInstOpts op
              dsts <- pDsts op
              pIntTernary loc prd op opts dsts []
            Op "STG" -> do
              opts <- pInstOpts op
              pST loc prd op opts
            Op "LDG" -> do
              opts <- pInstOpts op
              pLD loc prd op opts
            Op "ISETP" -> do
              opts <- pInstOpts op
              dst <- pDst
              pSymbol_ ","
              c_pred <- pSrcP
              pSymbol_ ","
              unresolved_srcs <- pSrcsN 2 pc op
              x_pred_srcs <- pOptPreds
              pCompleteInst
                loc
                prd
                op
                opts
                [dst]
                (resolved c_pred:unresolved_srcs)
                x_pred_srcs
            Op "NOP" -> do
              pCompleteInst loc prd op [] [] [] []
            _ -> do
              opts <- pInstOpts op
              dsts <- pDsts op
              unless (null dsts) $ do
                pSymbol_ ","
              unresolved_srcs <- pSrcs pc op
              pCompleteInst loc prd op opts dsts unresolved_srcs []

        resolved :: a -> Unresolved a
        resolved = const . Right
        resolveds :: [a] -> [Unresolved a]
        resolveds = map resolved

        -- pIntTernary :: PI (Unresolved Inst)
        pIntTernary loc prd op opts dsts src_pfx_preds = do
          -- TODO: try and parse up to two predicates before the regular
          -- sources, we can store them separate
          -- TODO: fix the instruction formatter if I remove these
          --
          pSymbol ","
          unresolved_srcs <- pSrcsN 3 pc op
          x_pred_srcs <- pOptPreds
            -- if InstOptX`elem`opts then do
            --   p0 <- pSymbol "," >> pPredSrcP
            --   p1 <- pSymbol "," >> pPredSrcP
            --   return [p0,p1]
            -- else return [PredP True PT,PredP True PT]
          pCompleteInst loc prd op opts dsts (src_pfx_preds++unresolved_srcs) x_pred_srcs

        pLD :: Loc -> Pred -> Op -> [InstOpt] -> PI (Unresolved Inst)
        pLD loc prd op opts = do
          let pDstPred = do
                p <- pDstP
                pSymbol_ ","
                return [p]
          dst_p <- if InstOptZD`elem`opts then pDstPred else return []
          dst_r <- pDstR
          pSymbol_ ","
          let pBareSrcR = SrcR False False False <$> pSyntax
          pSymbol "["
          r_addr <- pBareSrcR
          -- TODO: UR# goes in middle
          let pImmOffs = do
                ur <- P.option URZ $ P.try (pSymbol "+" >> pSyntax)
                i <- P.option (SrcI 0) $ P.try (pSymbol "+" >> pSrcI op)
                return (SrcUR False False ur,i)
          (ur_off,i_off) <- pImmOffs
          pSymbol "]"
          pCompleteInst
            loc prd op opts (dst_p++[dst_r]) (resolveds [r_addr,ur_off,i_off]) []

        pST :: Loc -> Pred -> Op -> [InstOpt] -> PI (Unresolved Inst)
        pST loc prd op opts = do
          let pBareSrcR = SrcR False False False <$> pSyntax
          pSymbol "["
          r_addr <- pBareSrcR
          -- TODO: UR# goes in middle
          let pImmOffs = do
                ur <- P.option URZ $ P.try (pSymbol "+" >> pSyntax)
                i <- P.option (SrcI 0) $ P.try (pSymbol "+" >> pSrcI op)
                return (SrcUR False False ur,i)
          (ur_off,i_off) <- pImmOffs
          pSymbol "]"
          pSymbol ","
          r_data <- pBareSrcR
          pCompleteInst loc prd op opts [] (resolveds [r_addr,ur_off,i_off,r_data]) []


        -- up to two predicate sources
        pOptPreds = do
          P.option [] $ do
            p_src0 <- pSymbol "," >> pPredSrcP
            P.option [p_src0] $ do
              p_src1 <- pSymbol "," >> pPredSrcP
              return [p_src0,p_src1]

        pCompleteInst :: Loc -> Pred -> Op -> [InstOpt] -> [Dst] -> [Unresolved Src] -> [Pred] -> PI (Unresolved Inst)
        pCompleteInst loc prd op opts dsts unresolved_srcs src_preds = do
          dep_info <- pDepInfo
          P.try $ pSymbol ";"
          return $ \lbls -> do
            srcs <- sequence (map ($lbls) unresolved_srcs)
            return $
              Inst {
                iLoc = loc
              , iPc = pc
              , iPredication = prd
              , iOp = op
              , iOptions = opts
              , iDsts = dsts
              , iSrcs = srcs
              , iSrcPreds = src_preds
              , iDepInfo = dep_info
              }


pPred :: PI Pred
pPred = do
    pSymbol "@"
    sign <- P.option False $ pSymbol "!" >> return True
    tryP sign <|> tryUP sign
  where tryP sign = PredP sign <$> pSyntax
        tryUP sign = PredUP sign <$> pSyntax

pPredSrcP :: PI Pred
pPredSrcP = do
    sign <- P.option False $ pSymbol "!" >> return True
    tryP sign
  where tryP sign = PredP sign <$> pSyntax



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
          | op == Op "IMAD" = pSymbols ["MOV","SHL","IADD"] >> return []
          -- | op == Op "IMAD" = pSymbols [] >> return []
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
              dp <- pDstP
              pSymbol ","
              return [dp]
          | otherwise = return []



pDst :: PI Dst
pDst = pDstR <|> pDstP <|> pDstUP <|> pDstUR <|> pDstB
pDstR :: PI Dst
pDstR = pLabel "reg" $ DstR <$> pSyntax
pDstB :: PI Dst
pDstB = pLabel "barrier" $ DstB <$> pSyntax
pDstP :: PI Dst
pDstP = pLabel "pred reg" $ DstP <$> pSyntax
pDstUP :: PI Dst
pDstUP = pLabel "unif pred reg" $ DstUP <$> pSyntax
pDstUR :: PI Dst
pDstUR = pLabel "unif reg" $ DstUR <$> pSyntax

pSrcsN :: Int -> PC -> Op -> PI [Unresolved Src]
pSrcsN 0 _  _  = return []
pSrcsN n pc op = do
  src0 <- pSrc pc op
  srcs <- sequence $ replicate (n - 1) (pSymbol "," >> pSrc pc op)
  return (src0:srcs)
pSrcs :: PC -> Op -> PI [Unresolved Src]
pSrcs pc op = P.sepBy (pSrc pc op) (pSymbol ",")
pSrc :: PC -> Op -> PI (Unresolved Src)
pSrc pc op = P.try pNonLabel <|> pImmLbl
  where pNonLabel :: PI (Unresolved Src)
        pNonLabel = alwaysSucceeds <$> pCases
          where pCases =
                  P.try (pSrcI op) <|>
                  pSrcCCX <|>
                  pSrcR <|>
                  pSrcSR <|>
                  pSrcUR <|>
                  pSrcUP <|>
                  pSrcP <|>
                  pSrcB

        alwaysSucceeds :: Src -> Unresolved Src
        alwaysSucceeds src _ = Right src

        pImmLbl :: PI (Unresolved Src)
        pImmLbl = pLabel "label expression" $ do
          e <- pLExpr pc
          return $ \lix -> SrcI <$> evalLExpr lix e


pSrcCCX :: PI Src
pSrcCCX = pLabel "const surface" $ P.try pConstInd <|> P.try pConstDir
  where pConstDir :: PI Src
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

-- rejects 32@hi... and 32@lo...
pSrcI :: Op -> PI Src
pSrcI = pLabel "imm" . pSrcImmNonBranch
pSrcImmNonBranch :: Op -> PI Src
pSrcImmNonBranch op = do
    imm <- pVal
    P.notFollowedBy (P.char '@')
    pWhiteSpace
    return $ SrcI imm
  where pVal
          | oOpIsFP op = pFltImm32 <|> pIntImm32
          | otherwise = pIntImm32

pNegationNumeric :: PI Bool
pNegationNumeric = P.option False $ (pSymbol "-" <|> pSymbol "~") >> return True
pNegationLogical :: PI Bool
pNegationLogical = P.option False (pSymbol "!" >> return True)

-- how to make this work with <$>...<*>...<*>...
-- pWithNegAbsC :: PI a -> PI (Bool -> Bool -> PI a)

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

pSrcR :: PI Src
pSrcR = pLabel "reg" $ do
  (r,neg,abs) <- pWithNegAbs pSyntax
  reuse <- P.option False $ P.try (pSymbol ".reuse" >> return True)
  return $ SrcR neg abs reuse r

pSrcSR :: PI Src
pSrcSR = pLabel "sys reg" $ SrcSR <$> (P.try pUnderbarTidCtaid <|> pSyntax)
  where pUnderbarTidCtaid =
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
pSrcUR = pLabel "unif reg" $ do
  (ur,neg,abs) <- pWithNegAbs pSyntax
  return (SrcUR neg abs ur)

pSrcP :: PI Src
pSrcP = pLabel "pred reg" $
  SrcP <$> pNegationLogical <*> pSyntax

pSrcUP :: PI Src
pSrcUP = pLabel "pred reg" $
  SrcUP <$> pNegationLogical <*> pSyntax

pSrcB :: PI Src
pSrcB = pLabel "barrier reg" $ SrcB <$> pSyntax


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
  | LExprNeg   !Loc !LExpr -- -E
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

pIntImm32 :: PI Word64
pIntImm32 = do
  -- Were we to use Int32, then fromIntegral to Word64 would sign extend
  -- and create a value that won't fit in encoding.
  --
  -- The function negate :: Word32 -> Word32 is still two's-complement
  let pImmS32 = pImmA :: PI Word32
  maybeNegate <- P.option id (pSymbol "-" >> return negate)
  fromIntegral . maybeNegate <$> pImmS32

pFltImm32 :: PI Word64
pFltImm32 = do
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
                pMaybeSep = unless (ix == 0) $ pSymbol_ ","
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

