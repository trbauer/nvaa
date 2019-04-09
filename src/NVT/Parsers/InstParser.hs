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

parseInst :: PC -> FilePath -> String -> Either Diagnostic (Inst,[Diagnostic])
parseInst pc = runPI (pInst pc)

runPI :: PI a -> FilePath -> String -> Either Diagnostic (a,[Diagnostic])
runPI p = runPID p ()
testPI :: Show a => PI a -> String -> IO ()
testPI pa inp = 
  case runPI (pa <* P.eof) "<interactive>" inp of
    Left d -> putStrLn $ dFormat d
    Right (a,_) -> print a

type PI a = PID () a


pInst :: PC -> PI Inst
pInst pc = body
  where body = pWithLoc $ \loc -> do
          pWhiteSpace
          prd <- pPred
          op <- pOp
          opts <- pInstOpts          
          dsts <- pDsts op
          -- unless (null dsts || not (oHasSrcs op)) $
          P.try $ pSymbol ","
          srcs <- pSrcs op
          dep_info <- pDepInfo
          let i = 
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
          P.try $ pSymbol ";"
          return i

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

pInstOpts :: PI [InstOpt]
pInstOpts = P.try pLop3Code <|> P.many (pSymbol "." >> pSyntax)
  where pLop3Code :: PI [InstOpt]
        pLop3Code = do
          pSymbol "."
          _ <- pLop3Expr
          return []

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


pSrcs :: Op -> PI [Src]
pSrcs op = P.sepBy (pSrc op) (pSymbol ",")
pSrc :: Op -> PI Src
pSrc op = 
    P.try pSrcConstInd <|> 
      P.try pSrcConstDir <|> 
        pSrcReg <|> 
        pSrcRegU <|> 
        pSrcRegP <|>
        pSrcRegB <|>
        pSrcImm
  where pWithNegAbs :: PI a -> PI (a,Bool,Bool)
        pWithNegAbs pBody = do
          neg <- P.option False $ pSymbol "-" >> return True
          let pWithAbs = do
                  pSymbol "|"
                  r <- pBody
                  pSymbol "|"
                  return (r,True)
          (r,abs) <- pWithAbs <|> (pBody >>= \r -> return (r,False))
          return (r,neg,abs)

        pSrcReg :: PI Src
        pSrcReg = do
          (r,neg,abs) <- pWithNegAbs pSyntax
          reuse <- P.option False $ P.try (pSymbol ".reuse" >> return True)
          return $ SrcR neg abs reuse r

        pSrcConstDir :: PI Src
        pSrcConstDir = do
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

        pSrcConstInd :: PI Src
        pSrcConstInd = do
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

        pSrcRegU :: PI Src
        pSrcRegU = SrcU <$> pSyntax

        pSrcRegP :: PI Src
        pSrcRegP = do
          neg <- P.option False (pSymbol "!" >> return True)
          SrcP neg <$> pSyntax

        pSrcRegB :: PI Src
        pSrcRegB = SrcB <$> pSyntax

        pSrcImm :: PI Src
        pSrcImm
          | "F"`isPrefixOf`(oMnemonic op) = SrcI <$> (pFltImm <|> pIntImm)
          | otherwise = SrcI <$> pIntImm


oHasDstPred :: Op -> Bool
oHasDstPred (Op op) = 
  op `elem` ["LOP3","PLOP3","DSETP","FSETP","ISET","HSETP2"]

oOpIsFP :: Op -> Bool
oOpIsFP (Op op) = 
  op `elem` ["FADD","FFMA","FCHK","FMNMX","FMUL","FSEL","FSET","FSETP"] ||
  op `elem` ["DADD","DFMA","DMUL","DSETP"]

-- oHasSrcs :: Op -> Bool
-- oHasSrcs op = 

pIntImm :: PI Int64
pIntImm = do
    maybeNegate <- P.option id (pSymbol "-" >> return negate)
    maybeNegate <$> pImmA

pFltImm :: PI Int64
pFltImm = do
    maybeNegate <- P.option id $ pSymbol "-" >> return (flip complementBit 31)
    maybeNegate <$> (pInf <|> pNonInf) 
  where pInf = pSymbol "INF" >> return 0x7FF00000
        pNonInf = fromIntegral . floatToBits . doubleToFloat <$> pFloating


-- {!8,Y,+2.R,+3.W,^4,^1}
pDepInfo :: PI DepInfo
pDepInfo = (pSymbol "{" >> pTokenLoop 0 di0 ) <|> return di0
  where di0 = DepInfo 0 False 0 Nothing Nothing
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

