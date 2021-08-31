module NVT.PTX.PTXParser where

import NVT.Diagnostic
import NVT.Loc
import NVT.Parsers.Parser
import NVT.PTX.PTXTypes

import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Text.Parsec((<|>),(<?>))
import qualified Text.Parsec           as P
import qualified Text.Parsec.Language  as P
import qualified Text.Parsec.Pos       as P
import qualified Text.Parsec.Token     as P

import System.Directory
import Util




parseListing :: FilePath -> String -> Either Diagnostic PTXListing
parseListing fp inp =
  case runPID (pListing <* P.eof) () fp inp of
    Left d -> Left d
    Right (a,_,_) -> return a


testPPTX :: Show a => PPTX a -> String -> IO ()
testPPTX par0 inp = do
  let par = do
        a <- par0
        sfx <- P.getInput
        return (a,sfx)
  case runPID par () "<interactive>" inp of
    Left d -> putStrLn $ dFormatWithLines (lines inp) d
    Right ((a,sfx),_,_) -> do
      print a
      unless (null sfx) $
        putStrLn $ "with suffix: " ++ show sfx

type PPTX a = PID () a

pListing :: PPTX PTXListing
pListing = do
  pWhiteSpace
  --
  pKeyword ".version"
  maj <- pInt
  pSymbol "."
  min <- pInt
  --
  pKeyword ".target"
  sm_ver <- pSmVer
  topts <-
    P.option [] $ do
      pSymbol ","
      (\c -> [c]) <$> pIdentifier
  --
  pKeyword ".address_size"
  addr_size <- pInt
  --
  (es,vs,fs,ss) <- pListingElems
  --
  return $
    PTXListing {
      plVersion = (maj,min)
    , plTarget = (sm_ver,topts)
    , plAddrSize = addr_size
    , plFunctions = es
    , plProgramScope = vs
    , plLocFiles = fs
    , pSections = ss
    }

-- TODO: https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#variables
--   .global .u32 loc;
--   .reg    .s32 i;
--   .const  .f32 bias[] = {-1.0, 1.0};
--   .global .u8  bg[4] = {0, 0, 0, 0};
--   .reg    .v4 .f32 accel;
--   .reg    .pred p, q, r;
--   .global .u32 index[] = { 0, 1, 2, 3, 4, 5, 6, 7 };
--   .global .s32 offset[][2] = { {-1, 0}, {0, -1}, {1, 0}, {0, 1} };
pListingElems :: PPTX ([PTXFunction],[PTXProgramScopeVariable],[PTXFile],[PTXSection])
pListingElems = loop [] [] [] []
  where loop es vs fs ss =
            P.try pE <|> P.try pV <|> pF <|> pS <|> pEnd
          where pE = do
                  e <- pEntryOrFunction
                  loop (e:es) vs fs ss
                pV = do
                  v <- pProgramScopeVariables
                  loop es (reverse v ++ vs) fs ss
                pF = do
                  f <- pFileDirective
                  loop es vs (f:fs) ss
                pS = do
                  s <- pSection
                  loop es vs fs (s:ss)
                pEnd = return (reverse es,reverse vs,reverse fs,reverse ss)


pSmVer :: PPTX SMVer
pSmVer = pEnum "sm version" (map toLower . show)

pEnum :: Enum a => String -> (a -> String) -> PPTX a
pEnum what fmt = do
  let es = [toEnum 0 ..]
  pOneOfNamed what $ map (\e -> (fmt e,e)) es


pEntryOrFunction :: PPTX PTXFunction
pEntryOrFunction = do
  mlkg <- P.try (P.optionMaybe pLinkage)
  pEntry mlkg <|> pFunction mlkg

pEntry :: Maybe PTXLinkage -> PPTX PTXFunction
pEntry mlkg = pWithLoc $ \loc -> do
  P.try (pKeyword ".entry")
  pCompleteEntryOrFunction loc True Nothing mlkg

-- .func (.reg .u32 %res) inc_ptr ( .reg .u32 %ptr, .reg .u32 %inc )
pFunction :: Maybe PTXLinkage -> PPTX PTXFunction
pFunction mlkg = pWithLoc $ \loc -> do
  P.try (pKeyword ".func")
  m_ret <-
    P.optionMaybe $ do
      pSymbol "("
      ret <- pParam
      pSymbol ")"
      return ret
  pCompleteEntryOrFunction loc False m_ret mlkg

pCompleteEntryOrFunction ::
  Loc -> Bool ->
  Maybe PTXAlloc ->
  Maybe PTXLinkage ->
  PPTX PTXFunction
pCompleteEntryOrFunction loc is_entry m_ret mlnkg = do
  name <- pIdentifier
  pSymbol "("
  params <- pParam `P.sepBy` pSymbol ","
  pSymbol ")"
  ps <- P.many pPerf
  let pFuncDef = do
        pSymbol "{"
        es <- pElems
        pSymbol "}"
        return $
            PTXFunction {
              pfLoc = loc
            , pfReturn = m_ret
            , pfName = name
            , pfIsEntry = is_entry
            , pfParams = params
            , pfElems = es
            , pfLinkage = mlnkg
            , pfPerfDirectives = ps
            }
  let pFuncDecl = do
        pSymbol ";"
        return $
            PTXFunction {
              pfLoc = loc
            , pfReturn = m_ret
            , pfName = name
            , pfIsEntry = is_entry
            , pfParams = params
            , pfElems = []
            , pfLinkage = mlnkg
            , pfPerfDirectives = ps
            }
  pFuncDef <|> pFuncDecl

pPerf :: PPTX PPTXPerf
pPerf =
    pMaxreg <|> pMaxntid <|> pReqntid <|> pMinctapersm <|> pMaxctapersm <|>
        pNoreturn <|> pPragma
  where pMaxreg = p1 ".maxnreg" PPTXPerfMaxnreg
        pMaxntid = pDim ".maxntid" PPTXPerfMaxntid
        pReqntid = pDim ".reqntid" PPTXPerfReqntid
        pMinctapersm = p1 ".minnctapersm" PPTXPerfMinctapersm
        pMaxctapersm = p1 ".maxnctapersm" PPTXPerfMaxctapersm
        pNoreturn = p0 ".noreturn" PPTXPerfNoreturn
        pPragma = do
          P.try (pKeyword ".pragma")
          PPTXPerfPragma <$> pStringLiteral `P.sepBy1` pSymbol ","

        p0 kw cons = do
          P.try (pKeyword kw)
          return cons

        p1 kw cons = do
          P.try (pKeyword kw)
          cons <$> pInt

        pDim kw cons = do
          P.try (pKeyword kw)
          x <- pInt
          (x,y,z) <-
            P.option (x,1,1) $ do
              pSymbol ","
              y <- pInt
              P.option (x,y,1) $ do
                pSymbol ","
                z <- pInt
                return (x,y,z)
          return $ cons x y z

-- .const .align 8 .b8 __cudart_sin_cos_coeffs[128] = {186, 94 ...}
pProgramScopeVariables :: PPTX [PTXProgramScopeVariable]
pProgramScopeVariables = pWithLoc $ \loc -> do
  mlkg <- P.optionMaybe pLinkage
  as <- pAllocs
  mexpr <- P.optionMaybe $ pSymbol_ "=" >> pExpr
  pSymbol_ ";"
  let cons a =
        PTXProgramScopeVariable {
        pvLoc = loc
        , pvAlloc = a
        , pvInit = mexpr
        , pvLinkage = mlkg
        }
  return $ map cons as

pLinkage :: PPTX PTXLinkage
pLinkage =
  pOneOfKeyword "linkage"
    [ (".extern",PTXLinkageExtern)
    , (".visible",PTXLinkageVisible)
    , (".weak",PTXLinkageWeak) ]



-- .param .type .ptr .space .align N  varname
-- .param .type .ptr        .align N  varname << generic addr space
--
-- .space = { .const, .global, .local, .shared };
--
-- EXAMPLES:
--  .param .u32 .ptr .global .align 4 collatz_param_0,
--  .local .align 4 .b8  __local_depot3[4];
--  .param .b32 len (from CUDA PTX docs)
--  .global .attribute(.managed) .s32 g;
pAlloc :: PPTX PTXAlloc
pAlloc = do
  [pa] <- pAllocsG False
  return pa
-- allows comma separated
pAllocs :: PPTX [PTXAlloc]
pAllocs = pAllocsG True

pAllocsG :: Bool -> PPTX [PTXAlloc]
pAllocsG allow_multi = do
  sc <- P.try pSpace
  m_align <- P.optionMaybe  (P.try (pKeyword ".align") >> pInt)
  ty <- pType
  ptr <- P.option Nothing $ do
            P.try (pKeyword ".ptr")
            sp <- P.option PTXSpaceGeneric pSpace
            align <- P.try (pKeyword ".align") >> pInt
            return $ Just (sp,align)
  names <-
    if allow_multi then (pIdentifier <|> pRegIdent) `P.sepBy1` pSymbol_ ","
      else (\c -> [c]) <$> (pIdentifier <|> pRegIdent)
  repl <- P.option 1 $ do
    pSymbol "<"
    x <- pInt
    pSymbol ">"
    return x
  tydim <- pTypeDim ty
  let cons nm =
        PTXAlloc {
        paSpace = sc
        , paAlignment = m_align
        , paType = tydim
        , paPointer = ptr
        , paName = nm
        , paReplicate = repl
        }
  return $ map cons names


pTypeDim :: PTXType -> PPTX PTXTypeDim
pTypeDim pt = do
  vdim <- P.option False $ P.try (pSymbol "[" >> pSymbol "]") >> return True
  cdims <-
    P.many $ do
      pSymbol "["
      x <- pInt
      pSymbol "]"
      return x
  return $ PTXTypeDim pt vdim cdims


-- .param .u32 .ptr .global .align 4 saxpy_param_0,
-- .param .f32 saxpy_param_1,
--
-- (.param .align 16 .b8 func_retval0[16]) << int4 return
-- (16B aligned 16)
pParam :: PPTX PTXAlloc
pParam = pAlloc

pSpace :: PPTX PTXSpace
pSpace =
    pEnum "state space"
      (("." ++) . map toLower . drop (length "PTXSpace") . show)

pType :: PPTX PTXType
pType = do
  let ps :: [(String,PTXType)]
      ps = map (\t -> (fmt t,t)) ptx_types_all
        where fmt = ("." ++) . map toLower . drop (length "PTXType") . show
  pOneOfKeyword "type" ps

pElems :: PPTX [PTXElem]
pElems = concat <$> P.many pElem
  where pElem :: PPTX [PTXElem]
        pElem =
            tryOne pElemInst <|>
            tryOne pElemLabelOrCallPrototype <|>
            P.try pElemAllocs <|>
            tryOne pElemLocDef <|>
            tryOne pElemPragma <|>
            tryOne pElemScope

        tryOne p = (\c -> [c]) <$> P.try p

pElemAllocs :: PPTX [PTXElem]
pElemAllocs = pWithLoc $ \loc -> do
    map (PTXElemAlloc loc) <$> pAllocs <* pSymbol_ ";"

pElemInst :: PPTX PTXElem
pElemInst = pWithLoc $ \loc -> PTXElemInst loc <$> pInst

pElemPragma :: PPTX PTXElem
pElemPragma = pWithLoc $ \loc -> PTXElemPragma loc <$> par
  where par = do
          pKeyword ".pragma"
          pStringLiteral `P.sepBy1` pSymbol_ "," <* pSymbol_ ";"



-- prototype_0 : .callprototype (.param .b32 _) _ (.param .b32 _, .param .b32 _) ;
--  prototype_10 : .callprototype ()_ (.param .b64 _, .param .b32 _) ;
pElemLabelOrCallPrototype :: PPTX PTXElem
pElemLabelOrCallPrototype = pWithLoc $ \loc -> do
    lbl <- pLabel
    pCallPrototype loc lbl <|> return (PTXElemLabel loc lbl)
  where pLabel = pIdentifier <* pSymbol ":"
        pCallPrototype loc lbl = do
            P.try (pKeyword ".callprototype")
            mret <-
              P.option Nothing $ do
                pSymbol "("
                P.optionMaybe pAlloc <* pSymbol ")"
                            --
            _ <- pIdentifier -- usually an _
            --
            pSymbol "("
            params <- pAlloc `P.sepBy` pSymbol_ ","
            pSymbol ")"
            pSymbol ";"
            --
            return $
              PTXElemProto loc (PTXPrototype lbl mret params)

pElemLocDef :: PPTX PTXElem
pElemLocDef = pWithLoc $ \loc -> PTXElemLocDef loc <$> pLoc

pElemScope :: PPTX PTXElem
pElemScope = pWithLoc $ \loc -> do
    pSymbol "{"
    es <- pElems
    pSymbol "}"
    return $ PTXElemScope loc es

--  .loc 2 16 0
--  .loc 2 3 0, function_name info_string0, inlined_at 2 18 0
pLoc :: PPTX PTXLoc
pLoc = do
  P.try (pKeyword ".loc")
  fno <- pInt
  lno <- pInt
  col <- pInt
  attrs <-
    P.option ("",0,(0,0,0)) $ do
        pSymbol ","
        P.try (pKeyword "function_name")
        off <- P.option 0 $ pSymbol "+" >> pInt
        func <- pIdentifier
        pSymbol ","
        P.try (pKeyword "inlined_at")
        fno <- pInt
        lno <- pInt
        col <- pInt
        return (func,off,(fno,lno,col))

  return $ PTXLoc fno lno col attrs

-- 	st.param.b32	[func_retval0+0], %r4;
-- 	setp.eq.s32	%p1, %r22, 1;
--	@%p1 bra 	BB0_5;
-- 	ld.param.v4.b32	{%r25, %r26, %r27, %r28}, [retval0+0];
pInst :: PPTX PTXInst
pInst = pWithLoc $ \loc -> do
    pred <- P.optionMaybe pPred
    op <- pOp
    if "call"`isPrefixOf`op
      then pCallInst loc pred op else pNormalInst loc pred op
  where pNormalInst loc pred op = do
          m_dst <-
            if not (hasDst op) then return Nothing
            else Just <$> pDst op (-1)
          let pSrcs
                | hasDst op = P.many (pSymbol "," >> pSrc op 0)
                | otherwise = pSrc op 0 `P.sepBy` pSymbol ","
          srcs <- P.option [] pSrcs
          pSymbol ";"
          return $
            PTXInst {
              piLoc = loc
            , piPred = pred
            , piOp = op
            , piDst = m_dst
            , piSrcs = srcs
            }

        -- call.uni (retval0),  func2, (param0, param1);
        -- call (%r1), inc_ptr, (%r1,4);
        -- call (%r1), inc_ptr, (%r1,4), prototype_0;
        pCallInst loc pred op = do
          ret <- P.option [] $ do
            pSymbol_ "("
            ret <- pSrc op 0
            pSymbol_ ")"
            pSymbol ","
            return [PTXOperandRval ret]
          func <- pSrc op 0 <* pSymbol ","
          pSymbol_ "("
          args <- pSrc op 0 `P.sepBy` pSymbol ","
          pSymbol_ ")"
          proto <- P.option [] $ do
              pSymbol ","
              (\c -> [PTXOperandSym c]) <$> pIdentifier
          pSymbol ";"
          return $
            PTXInst {
              piLoc = loc
            , piPred = pred
            , piOp = op
            , piDst = Nothing
            , piSrcs = ret ++ [func] ++ args ++ proto
            }

hasDst :: PTXOp -> Bool
hasDst op = not $ any (`isPrefixOf`op) no_dst
  where no_dst =
          [
            "bar.sync" -- e.g. bar sync
          , "bar.warp.sync"
          , "barrier.sync"
          , "bra"
          , "call"
          , "cp.async.commit_group"
          , "cp.async.mbarrier.arrive"
          , "cp.async.wait_group"
          , "fence."
          , "membar."
          , "mbarrier.arrive"
          , "nanosleep."
          , "ret"
          , "red." -- reduce has no read back
          , "st."
          , "trap"
          ]

-- hasSrcs :: PTXOp -> Bool
-- hasSrcs op = not $ any (`isPrefixOf`op) no_srcs
--   where no_srcs =
--           [
--             "activemask"
--           , "cp.async.commit_group"
--           , "ret"
--           , "trap"
--           ]


-- a little different than other identifiers because it allows stuff like
-- tex.2d.v4...
--     ^ number
-- also the first token is a little more constrained
pOp :: PPTX PTXOp
pOp = do
  op0 <- P.letter
  let pIdentChar = (P.alphaNum <|> P.oneOf "_")
  op_sfx <- P.many pIdentChar
  sfxs <- P.many (P.char '.' >> P.many1 pIdentChar)
  pWhiteSpace
  return $ op0:op_sfx ++ concatMap ("." ++) sfxs

-- r0  (inline asm uses this)
-- %r0
-- %r0.suffix
pVarIdentifier :: PPTX String
pVarIdentifier = do
  pfx <- P.option "" $ P.char '%' >> return "%"
  (pfx ++) <$> pIdentifierWithDots

pIdentifierWithDots :: PPTX String
pIdentifierWithDots = do
  op <- pIdentifier
  sfxs <-
    P.many $ P.char '.' >> pIdentifier
  return $ op ++ concatMap ("." ++) sfxs

-- e.g. @!%p2 ...
pPred :: PPTX (Bool,PTXReg)
pPred = do
  P.char '@'
  neg <- P.option False $ pSymbol "!" >> return True
  preg <- pVarIdentifier
  return (neg,preg)

pDst :: PTXOp -> Int -> PPTX PTXOperand
pDst op opnd_ix = do
  dst <- pOperand op opnd_ix
  let pWithPred = do
        pSymbol_ "|"
        dst_pr <- pOperand op opnd_ix
        return $ PTXOperandWithPred dst dst_pr

  pWithPred <|> return dst

pSrc :: PTXOp -> Int -> PPTX PTXOperand
pSrc = pOperand

pRegIdent :: PPTX String
pRegIdent = do
  P.char '%'
  reg <- pIdentifierWithDots
  return ('%':reg)

-- [func_retval0+0]
-- %r4
-- %p1
-- BB0_5
-- {%r25, %r26, %r27, %r28}
-- 1
-- 0f3F800000
-- [%rd8, {%f2, %f3}] (e.g texture)
pOperand :: PTXOp -> Int -> PPTX PTXOperand
pOperand op opnd_ix =
    pReg <|> pLbl <|> pVec <|> P.try pImmF <|> pImmI <|> P.try pAddrT <|> pAddr
  where pReg = PTXOperandReg <$> pRegIdent -- these will be in conflict

        pLbl = PTXOperandSym <$> pIdentifier --

        pImmF = pWithLoc $ \loc ->
            PTXOperandImm <$> do
              P.char '0' >> (P.char 'f' <|> P.char 'd')
              ds <- P.many1 P.hexDigit
              pImmLoopUnsigned 16 loc ds
        pImmI = do
          modf <- P.option id $ pSymbol "-" >> return complement
          PTXOperandImm . modf <$> pImm64

        pVec = PTXOperandVec <$> do
          pSymbol "{"
          ops <- pOperand op opnd_ix `P.sepBy1` pSymbol ","
          pSymbol "}"
          return ops

        pAddrT = do
          pSymbol "["
          coord0 <- pOperand op opnd_ix
          coords <- P.many1 $ pSymbol "," >> pOperand op opnd_ix
          pSymbol "]"
          return $ PTXOperandTexAddr (coord0:coords)

        pAddr = do
          pSymbol "["
          addr <- pOperand op opnd_ix
          off <- P.option 0 $ do
              pSymbol "+"
              -- they can do +-0x1
              neg <- P.option id $ pSymbol "-" >> return negate
              neg <$> pInt
          pSymbol "]"
          return $ PTXOperandAddr addr off

-- .file  2 "D:\\work\\projects\\new-eu\\xet\\examples\\functions.cl"
-- .file  N ..., time, size
pFileDirective :: PPTX PTXFile
pFileDirective = do
  P.try (pKeyword ".file")
  fno <- pInt
  path <- pStringLiteral
  (ts,fsz) <- P.option (0,0) $ do
    pSymbol ","
    ts <- pImm64
    pSymbol ","
    fsz <- pInt
    return (ts,fsz)
  return $ PTXFile fno path ts fsz

pSection :: PPTX PTXSection
pSection = do
  P.try (pKeyword ".section")
  P.char '.'
  nm <- pIdentifierWithDots
  pSymbol "{"
  lns <- P.many pSectionLine
  pSymbol "}"
  return $
    PTXSection {
      psName = '.':nm
    , psSectionLines = lns
    }

pSectionLine :: PPTX PTXSectionLine
pSectionLine = pLbl <|> pLineW8 <|> pLineW16 <|> pLineW32 <|> pLineW64
  where pLbl = PTXSectionLineLabel <$> pIdentifier <* pSymbol ":"

        pLineW8  = pLine ".b8"  pImm8  PTXSectionLineW8

        pLineW16 = pLine ".b16" pImm16 PTXSectionLineW16
        pLineW32 = pLine ".b32" pImm32 PTXSectionLineW32
        pLineW64 = pLine ".b64" pImm64 PTXSectionLineW64

        pLine :: (Bits a,Num a) =>
          String -> PPTX a ->
          ([PTXInitValue a] -> PTXSectionLine) ->
          PPTX PTXSectionLine
        pLine ty pImm cons = do
          P.try (pKeyword ty)
          cons <$> (pElem pImm `P.sepBy` pSymbol_ ",")

        pElem pWord = pLblPlusImm <|> pImm
          where pLblPlusImm  = do
                  lbl <- P.try pSecName
                  off <- P.option 0 $ do
                      let signedNeg = (+ 1) . complement
                      func <- (pSymbol "+" >> return id) <|> (pSymbol "-" >> return signedNeg)
                      func <$> pWord
                  return $ PTXInitValue lbl off
                pImm = PTXInitValue "" <$> pWord

        -- .debug_info
        pSecName = do
          dot <- P.option "" $ P.char '.' >> return "."
          nm <- pIdentifierWithDots
          return (dot ++ nm)

pExpr :: PPTX PTXExpr
pExpr = pTop
  where pTop = pAdd

        pAdd = pBinOp pMul pAddOp
          where pAddOp =
                  (pSymbol "+" >> return PTXExprAdd) <|>
                    (pSymbol "-" >> return PTXExprSub)
        pMul = pBinOp pNeg pMulOp
          where pMulOp =
                  (pSymbol "*" >> return PTXExprAdd) <|>
                    (pSymbol "/" >> return PTXExprSub) <|>
                    (pSymbol "%" >> return PTXExprSub)

        pBinOp pNext pOp = do
          e0 <- pNext
          let pSfx e1 = do
                c <- pOp
                e2 <- pNext
                pSfx (c e1 e2) <|> return (c e1 e2)
          pSfx e0 <|> return e0

        pNeg = do
          cons <-
            P.option id $ do
                pSymbol "-" >> return PTXExprNeg
          cons <$> pAtom

        pAtom = pI <|> pL <|> pArr <|> pStr
          where pI = PTXExprImm <$> pImm64
                pL = PTXExprSym <$> pIdentifierWithDots

                pArr = do
                  pSymbol "["
                  as <- pExpr `P.sepBy` pSymbol_ ","
                  pSymbol "]"
                  return $ PTXExprArray as

                pStr = do
                  pSymbol "{"
                  as <- pExpr `P.sepBy` pSymbol_ ","
                  pSymbol "}"
                  return $ PTXExprStruct as


