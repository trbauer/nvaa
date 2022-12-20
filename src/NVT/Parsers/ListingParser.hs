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
    Right (i,_,_) -> do
      putStrLn $ fmtInstIr i
      let oup = format i
      putStrLn $ "  inp: " ++ inp
      putStrLn $ "  oup: " ++ oup
      case runPI (pWhiteSpace >> pInst 0) "<debug-reparsed>" oup of
        Left err -> do
          putStrLn $ "*** REPARSE FAILED"
          putStrLn $ dFormatWithLines (lines oup) err
        Right (i1,_,_) -> do
          case iDiffs i i1 of
            [] -> do
              putStrLn "(reparse succeeded)"
            (fnm,(v0,v1)):_ -> do
              putStrLn $ "*** REPARSED IR MISMATCH"
              putStrLn fnm
              putStrLn ("  parsed:   " ++ v0)
              putStrLn ("  reparsed: " ++ v1)
          case depsInpsOups i of
            (ins,ous) -> putStrLn $
              "ins: " ++ fmtRegSet ins ++ "\n" ++
              "ous: " ++ fmtRegSet ous ++ "\n" ++
              ""



--- just parse a bare list of instructions
-- parseInsts :: FilePath -> String -> Either Diagnostic ([Inst],LabelIndex,[Diagnostic])
-- parseInsts fp inp = do
--  case runPI (pWhiteSpace >> pInsts 0) fp inp of
--    Left err -> Left err
--    Right (is,pis,ws) -> return (is,pisLabelDefinitions pis,ws)

pListing :: PI [TextSection]
pListing = do
  sm_ver <- pListingHeader
  concat <$> P.many pSection

pSection :: PI [TextSection]
pSection = pTS <|> pOS
  where pTS :: PI [TextSection]
        pTS = do
          (knm,prot) <- P.try pTextSectionHeader
          txs <- pTextSectionBody knm prot
          return [txs]
        pOS = do
          P.try pOtherSectionHeader
          pOtherSectionBody
          return []

-- e.g.
-- .section  .nv.rel.action,"",@"SHT_CUDA_RELOCINFO"
pOtherSectionHeader :: PI ()
pOtherSectionHeader = do
  pKeyword ".section"
  lbl <- pLabelRef
  when (".text"`isPrefixOf`lbl) $ do
    fail "text section"
    return ()
  pSymbol ","
  slit <- pStringLiteral
  pSymbol ","
  P.try (pKeyword "@progbits") <|>
    do {P.char '@'; pStringLiteral; return ()} -- e.g. @"SHT_CUDA_RELOCINFO"
pOtherSectionBody :: PI ()
pOtherSectionBody = do
  a <- P.option 0 (P.try pAlign)
  sent_size <- P.option 0 (P.try (pKeyword ".sectionentsize" >> pInt))
  P.many pOtherSectionLine
  return ()
pOtherSectionLine :: PI ()
pOtherSectionLine =
    P.try (pLabelDef >> return ()) <|>
    P.try pShortLine <|>
    P.try pWordLine <|>
    P.try pDwordLine <|>
    P.try pZeroLine <|>
    P.try pAlgn <|>
    pOther
  where pOther = pByteLine <|> pDwordLine
        pByteLine = P.try (pSymbol ".byte") >> P.many (P.noneOf "\n\r") >> pWhiteSpace
        pShortLine = P.try (pSymbol ".short") >> P.many (P.noneOf "\n\r") >> pWhiteSpace
        pWordLine = P.try (pSymbol ".word") >> P.many (P.noneOf "\n\r") >> pWhiteSpace
        pDwordLine = P.try (pSymbol ".dword") >> P.many (P.noneOf "\n\r") >> pWhiteSpace
        pZeroLine = P.try (pSymbol ".zero") >> P.many (P.noneOf "\n\r") >> pWhiteSpace
        pAlgn = pAlign >> return ()


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
pAlign = pKeyword ".align" >> pInt

pTextSectionHeader :: PI (String,String)
pTextSectionHeader = do
  -- .section .text._Z14d_renderCatRomP6uchar4jjfffffy,"ax",@progbits
  pKeyword ".section"
  knm <- pSymbol ".text." >> pIdentifier
  pSymbol ","
  prot <- pStringLiteral -- e.g. "ax" ...
  pSymbol ","
  pKeyword "@progbits"
  return (knm,prot)

pTextSectionBody :: String -> String -> PI TextSection
pTextSectionBody knm prot = do
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
  P.eof <|> P.lookAhead (pTryKeyword ".section")
  --
  return $
    TextSection {
      tsKernelName = knm
    , tsRegisters = nregs
    , tsBarriers = nbars
    , tsAlignment = align
    , tsBlocks = bs
    }

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
    P.try (pSymbol ".weak")
    pLabelRef
    --
    pSymbol ".type"
    pLabelRef
    pSymbol ","
    pKeyword "@function"
    --
    pSymbol ".size"
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
