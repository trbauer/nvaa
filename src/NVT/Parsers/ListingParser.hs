module NVT.Parsers.ListingParser where

import NVT.Diagnostic
import NVT.Dataflow
import NVT.IR
import NVT.ListingTypes
import NVT.Parsers.InstParser
import NVT.Parsers.NVParser

import Control.Monad
import Data.Char
import Data.List
import Debug.Trace
import System.IO
import Text.Parsec((<|>),(<?>))
import qualified Text.Parsec           as P

{-
parseCubinListing :: FilePath -> String -> Either Diagnostic (CubinListing,LabelIndex)
parseCubinListing fp inp =
  case runPI pCubinListing fp inp of
    Left err -> Left err
    Right (ts,pis,_) -> return (ts,pisLabelDefinitions pis)


pCubinListing :: PI [TextSection]
pCubinListing = do
  sm_ver <- pListingHeader
  tss <- sequenceUnresolved . concat <$> P.many pListingElem
  --
  ds <- pisLabelDefinitions <$> pGet
  case tss ds of
    Left err -> pSemanticErrorRethrow err
    Right tss -> return tss
-}


-- parses a full cubin listing
parseListing :: FilePath -> String -> Either Diagnostic Listing
parseListing fp inp = (\(l,_,_) -> l) <$> parseListingG fp inp
parseListingG :: FilePath -> String -> Either Diagnostic (Listing,LabelIndex,[Diagnostic])
parseListingG fp inp = do
  case runPI (pWhiteSpace >> pListing) fp inp of
    Left err -> Left err
    Right (ts,pis,ws) -> return (Listing ts,pisLabelDefinitions pis,ws)

testInst :: String -> IO ()
testInst inp =
  case runPI (pWhiteSpace >> pInst 0) "<debug>" inp of
    Left err -> putStrLn $ dFormatWithLines (lines inp) err
    Right (i,_,_) -> testInstReparse True inp i >> return ()

testInstReparse :: Bool -> String -> Inst -> IO Bool
testInstReparse ff inp i = do
  let oup = format i
  when ff $ do
    putStrLn $ "  inp:    " ++ inp ++ "\n"
    putStrLn $ indent "  " $ fmtInstIr i
    putStrLn $ "  fmt(i): " ++ oup
  case runPI (pWhiteSpace >> pInst 0) "<debug-reparsed>" oup of
    Left err -> do
      putStrLn $ "*** REPARSE FAILED"
      putStrLn $ dFormatWithLines (lines oup) err
      return False
    Right (i1,_,_) -> do
      case iDiffs i i1 of
        [] -> do
--          unless ff $
--            putStrLn $ "(reparse succeeded): " ++ inp
          case depsInpsOups i of
            (ins,ous) -> do
              when ff $
                putStrLn $
                  "ins: " ++ fmtRegSet ins ++ "\n" ++
                  "ous: " ++ fmtRegSet ous ++ "\n" ++
                  ""
          return True
        (fnm,(v0,v1)):_ -> do
          putStrLn $ "*** REPARSED IR MISMATCH"
          putStrLn $ "  inp:    " ++ inp
          putStrLn $ "  fmt(i): " ++ oup

          putStrLn $ "FIELD: " ++ fnm
          putStrLn ("  parsed:   " ++ v0)
          putStrLn ("  reparsed: " ++ v1)
          return False

indent :: String -> String -> String
indent ind = unlines . map (ind ++) . lines

type KInfo = (String,[Int])

testListing :: FilePath -> String -> IO (Bool,[KInfo])
testListing sass_fp sass_inp =
    case parseListing sass_fp sass_inp of
      Left d -> do
        hPutStrLn stderr (dFormatWithLines sass_lns d)
        return (False,[])

      Right l -> testTss [] (lTextSections l)

  where sass_lns :: [String]
        sass_lns = lines sass_inp

        testTss :: [KInfo] -> [TextSection] -> IO (Bool,[KInfo])
        testTss rkis [] = return (True,reverse rkis)
        testTss rkis (ts:tss) = do
          let testBs :: [Block] -> IO Bool
              testBs [] = return True
              testBs (b:bs) = do
                -- let lbl = case bLabels b of {x:_ -> x; _->"???";}
                -- putStrLn $ lbl ++ ": // " ++ show (length (bInsts b)) ++ ": " ++ show (bLoc b)
                let testIs :: [Inst] -> IO Bool
                    testIs [] = return True
                    testIs (i:is) = do
                      let ln = sass_lns !! ((lLine (iLoc i)) - 1)
                      z <- testInstReparse False ln i
                      if z then testIs is else do
                        -- putStrLn ""
                        -- print (ts {tsBlocks = []})
                        -- print (b {bInsts = []})
                        testInstReparse True ln i
                        return False
                z <- testIs (bInsts b)
                if z then testBs bs else return False
          z <- testBs (tsBlocks ts)
          let blens = map (length . bInsts) (tsBlocks ts)
          sum blens`seq`return ()
          let rkis1 = (tsKernelName ts,blens):rkis
          if z then testTss rkis1 tss else return (False,reverse rkis1)


--- just parse a bare list of instructions
-- parseInsts :: FilePath -> String -> Either Diagnostic ([Inst],LabelIndex,[Diagnostic])
-- parseInsts fp inp = do
--  case runPI (pWhiteSpace >> pInsts 0) fp inp of
--    Left err -> Left err
--    Right (is,pis,ws) -> return (is,pisLabelDefinitions pis,ws)

pListing :: PI [TextSection]
pListing = do
  sm_ver <- pListingHeader
  tss <- concat <$> P.many pSection
  P.eof
  return tss

pSection :: PI [TextSection]
pSection = do
    -- pTraceLAK 64 "pSection>"
    pTS <|> pOS
  where pTS :: PI [TextSection]
        pTS = do
          -- pTraceLAK 32 "pTS>"
          (knm,prot) <- P.try pTextSectionHeader
          -- pTraceLAK 32 $ "pTS< " ++ knm
          txs <- pTextSectionBody knm prot
          return [txs]
        pOS = do
          -- pTraceLAK 32 "pOS>"
          sh <- P.try pOtherSectionHeader
          -- pTraceLAK 32 $ "pOS< " ++ sh
          pOtherSectionBody
          pCheckEofOrLookingAtNextSection
          return []

-- e.g.
-- .section  .nv.rel.action,"",@"SHT_CUDA_RELOCINFO"
pOtherSectionHeader :: PI String
pOtherSectionHeader = do
  pTryKeyword ".section"
  lbl <- pLabelRef
  when (".text"`isPrefixOf`lbl) $ do
    fail "text section" -- should be unreachable since pTS comes first
    return ()
  pSymbol ","
  slit <- pStringLiteral
  pSymbol ","
  let pProgBits = pTryKeyword "@progbits" >> return "@progbits"
  let pNoBits = pTryKeyword "@nobits" >> return "@nobits"
  let pAt = do {P.char '@'; s <- pStringLiteral; return ("@" ++ s)} -- e.g. @"SHT_CUDA_RELOCINFO"
  sfx <- pProgBits <|> pNoBits <|> pAt
  return (".section  " ++ lbl ++ ", " ++ show slit ++ ", " ++ sfx)
pOtherSectionBody :: PI ()
pOtherSectionBody = do
  a <- P.option 0 (P.try pAlign)
  sent_size <- P.option 0 (P.try (pKeyword ".sectionentsize" >> pInt))
  lbls <- P.many pOtherSectionLine
  return ()
pOtherSectionLine :: PI String
pOtherSectionLine = do
    -- pTraceLA "pOtherSectionLine>"
    l <- pLine
    -- pTraceLA "pOtherSectionLine<"
    return l
  where pLine =
          pByteLine <|> pShortLine <|> pWordLine <|> pDwordLine <|>
          pZeroLine <|> pTypeLine <|> pSizeLine <|>
          pAlgn <|> pLbl

        pLbl = do
          lbl <- P.try pLabelDef -- needs to reject .section prefix
          return (lbl ++ ":")
        pByteLine = pMiscLine ".byte"
        pShortLine = pMiscLine ".short"
        pWordLine = pMiscLine ".word"
        pDwordLine = pMiscLine ".dword"

        -- .zero   4
        -- .type softeningSquared,@object
        -- .size softeningSquared_fp64,(.L_1 - softeningSquared_fp64)
        pZeroLine = pMiscLine ".zero"
        pTypeLine = pMiscLine ".type"
        pSizeLine = pMiscLine ".size"

        pAlgn = do {a <- pAlign; return (".align " ++ show a)}

-- 	 .section  .nv.constant3,"a",@progbits
-- 	 .align  8
-- 	 .type   softeningSquared,@object
--   .size   softeningSquared,(.L_0 - softeningSquared)
-- softeningSquared:
--   .nv.constant3:
--   .zero  4
-- .L_0:
--   .zero  4
--   .type  softeningSquared_fp64,@object
--   .size  softeningSquared_fp64,(.L_1 - softeningSquared_fp64)
-- softeningSquared_fp64:
--   .zero  8
-- .L_1:

        pMiscLine :: String -> PI String
        pMiscLine pfx = do
          pTryKeyword pfx
          sfx <- pLineSfx
          return (pfx ++ " " ++ sfx)

        pLineSfx :: PI String
        pLineSfx = P.many (P.noneOf "\n\r") <* pWhiteSpace


--	.headerflags	@"EF_CUDA_TEXMODE_UNIFIED EF_CUDA_64BIT_ADDRESS EF_CUDA_SM80 EF_CUDA_VIRTUAL_SM(EF_CUDA_SM80)"
pListingHeader :: PI String
pListingHeader = pLabel "listing header (.headerflags ...)" $ do
  pKeyword ".headerflags"
  pSymbol "@"
  sm_ver <-
    pWithLoc $ \loc -> do
      ws <- words <$> pStringLiteral
      case filter ("EF_CUDA_SM"`isPrefixOf`) ws of
        [] -> pSemanticError loc ("cannot find sm version EM_CUDA_SM..")
        (sm:_) ->
          case drop (length "EF_CUDA_SM") sm of
            sm_num
              | sm_num`elem`supported_sms -> return ("sm_"++sm_num)
              where supported_sms = ["90", "86", "80", "75", "72", "70"]
            _ -> pSemanticError loc (sm ++ ": unsupported sm version")
  pKeyword ".elftype"
  pSymbol "@"
  pStringLiteral -- "ET_EXEC
  return sm_ver


pOtherChar :: PI [TextSection]
pOtherChar = P.anyChar >> pWhiteSpace >> return []

pAnyLine :: PI String
pAnyLine = pLabel "any line" $ do
    -- pTraceLA $ "pAnyLine.start"
    (pEmptyLine <|> pNonEmptyLine)
  where pEmptyLine = pEndLine >> return ""
        pNonEmptyLine = P.many1 (P.noneOf "\r\n") <* pEndLineOrEOF
        pEndLine = (P.crlf >> return ()) <|> (P.newline >> return ())
        pEndLineOrEOF = pEndLine <|> P.eof

pAlign :: PI Int
pAlign = pTryKeyword ".align" >> pInt

pTextSectionHeader :: PI (String,String)
pTextSectionHeader = do
  -- .section .text._Z14d_renderCatRomP6uchar4jjfffffy,"ax",@progbits
  pTryKeyword ".section"
  knm <- pSymbol ".text." >> pIdentifier
  pSymbol ","
  prot <- pStringLiteral -- e.g. "ax" ...
  pSymbol ","
  pKeyword "@progbits"
  return (knm,prot)

pTextSectionBody :: String -> String -> PI TextSection
pTextSectionBody knm prot = pWithLoc $ \ts_loc -> do
  --
  -- .sectionflags @"SHF_BARRIERS=1"
  nbars <-
    P.option 0 $ do
      pTryKeyword ".sectionflags"
      pSymbol "@"
      P.char '\"'
      pSymbol "SHF_BARRIERS="
      nbars <- pInt
      P.char '\"'
      pWhiteSpace
      return nbars
  -- .sectioninfo  @"SHI_REGISTERS=39"
  nregs <-
    P.option 0 $ do
      pTryKeyword ".sectioninfo"
      pSymbol "@"
      P.char '\"'
      pSymbol "SHI_REGISTERS="
      nregs <- pInt
      P.char '\"'
      pWhiteSpace
      return nregs

  align <- P.option 0 (P.try pAlign)
  --
  -- .global  _Z19d_renderFastBicubicP6uchar4jjfffffy
  -- .text._Z14timedReductionPKfPfPl: // e.g. not a global symbol??
  let pGlbSym = do
        pTryKeyword ".global"
        pLabelRef
      pLclSym = pLabelDef
  glb <- pGlbSym <|> pLclSym

  -- .type    _Z19d_renderFastBicubicP6uchar4jjfffffy,@function
  pKeyword ".type"
  sym <- pLabelRef
  pSymbol ","
  pSymbol "@function"
  -- .size    _Z19d_renderFastBicubicP6uchar4jjfffffy,(.L_164 - _Z19d_renderFastBicubicP6uchar4jjfffffy)
  pKeyword ".size"
  sym <- pLabelRef
  pSymbol ","
  pSymbol "("
  end <- pLabelRef
  pSymbol "-"
  start <- pLabelRef
  pSymbol ")"
  --  .other          _Z14d_renderCatRomP6uchar4jjfffffy,@"STO_CUDA_ENTRY STV_DEFAULT"
  P.option "" $ P.try $ do
    pKeyword ".other"
    sym <- pLabelRef
    pSymbol ","
    pSymbol "@"
    pStringLiteral
  --
  let setIds :: [Block] -> [Block]
      setIds = zipWith (\id b -> b {bId = id}) [1 ..]
  bs <- setIds <$> pBlocks 0
  --
  -- need to be at EOF or .section
  pCheckEofOrLookingAtNextSection
  --
  return $
    TextSection {
      tsLoc = ts_loc
    , tsKernelName = knm
    , tsRegisters = nregs
    , tsBarriers = nbars
    , tsAlignment = align
    , tsBlocks = bs
    }

pCheckEofOrLookingAtNextSection :: PI ()
pCheckEofOrLookingAtNextSection = do
  P.eof <|> P.lookAhead (pTryKeyword ".section")


pBlocks :: PC -> PI [Block]
pBlocks pc = pB <|> return []
  where pB = do
          b <- pBlock pc
          let next_pc =
                case bInsts b of
                  [] -> pc
                  _ -> iPc (last (bInsts b)) + 16
          (b:) <$> pBlocks next_pc

-- .weak  $_Z19d_renderFastBicubicP6uchar4jjfffffy$__cuda_sm3x_div_rn_noftz_f32_slowpath
-- .type  $_Z19d_renderFastBicubicP6uchar4jjfffffy$__cuda_sm3x_div_rn_noftz_f32_slowpath,@function
-- .size  $_Z19d_renderFastBicubicP6uchar4jjfffffy$__cuda_sm3x_div_rn_noftz_f32_slowpath,(.L_164 - $_Z19d_renderFastBicubicP6uchar4jjfffffy$__cuda_sm3x_div_rn_noftz_f32_slowpath)

pBlock :: PC -> PI Block
pBlock pc = pWithLoc $ \loc -> do
  P.option () $ do
    pTryKeyword ".weak"
    pLabelRef
    --
    pKeyword ".type"
    pLabelRef
    pSymbol ","
    pKeyword "@function"
    --
    pKeyword ".size"
    pLabelRef
    pSymbol ","
    pSymbol "("
    end <- pLabelRef
    pSymbol "-"
    st <- pLabelRef
    pSymbol ")"
    return ()
  lbl0 <- P.try pLabelDef
  lbls <- P.many (P.try pLabelDef)
  is <- pInsts pc
  return $
    Block {
      bId = 0
    , bLoc = loc
    , bLabels = lbl0:lbls
    , bInsts = is
    }
-- pKeyword ".section"
pInsts :: PC -> PI [Inst]
pInsts pc = pI <|> return []
  where pI = do
          i <- P.try (pInst pc)
          let i1 = i {iId = iPc i`div`16 + 1}
          (i1:) <$> pInsts (pc + 16)


pLabelDef :: PI String
pLabelDef = pLabelRef <* pSymbol ":"
pLabelRef :: PI String
pLabelRef = do
  let isLbL0 c =  isAlpha c || c`elem`".$_"
      isLblN c =  isAlphaNum c || c`elem`".$_"
  c0 <- P.satisfy isLbL0
  cs <- P.many (P.satisfy isLblN)
  pWhiteSpace
  return (c0:cs)
