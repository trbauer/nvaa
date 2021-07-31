module NVT.Parsers.ListingParser where

import NVT.Parsers.NVParser
import NVT.Parsers.InstParser
import NVT.IR
import NVT.Diagnostic

import Control.Monad
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

type TextSection = (String,[Inst])
-- data TextSection =
--  TextSection {tsName :: !String, tsInsts :: [Inst]}

-- parses a full cubin listing
parseListing :: FilePath -> String -> Either Diagnostic [TextSection]
parseListing fp inp = (\(ts,_,_) -> ts) <$> parseListingG fp inp

parseListingG :: FilePath -> String -> Either Diagnostic ([TextSection],LabelIndex,[Diagnostic])
parseListingG fp inp = do
  case runPI (pWhiteSpace >> pListing) fp inp of
    Left err -> Left err
    Right (ts,pis,ws) -> return (ts,pisLabelDefinitions pis,ws)

--- just parse a bare list of instructions
parseInsts :: FilePath -> String -> Either Diagnostic ([Inst],LabelIndex,[Diagnostic])
parseInsts fp inp = do
  case runPI (pWhiteSpace >> pInsts 0) fp inp of
    Left err -> Left err
    Right (is,pis,ws) -> return (is,pisLabelDefinitions pis,ws)

pListing :: PI [TextSection]
pListing = do
  sm_ver <- pListingHeader
  concat <$> P.many pListingElem

--	.headerflags	@"EF_CUDA_TEXMODE_UNIFIED EF_CUDA_64BIT_ADDRESS EF_CUDA_SM80 EF_CUDA_VIRTUAL_SM(EF_CUDA_SM80)"
pListingHeader :: PI String
pListingHeader = pLabel "listing header (.headerflags ...)" $ do
  pKeyword ".headerflags"
  pSymbol "@"
  pWithLoc $ \loc -> do
    ws <- words <$> pStringLiteral
    case filter ("EF_CUDA_SM"`isPrefixOf`) ws of
      [] -> pSemanticError loc ("cannot find sm version EM_CUDA_SM..")
      (sm:_) ->
        case drop (length "EF_CUDA_SM") sm of
          sm_num
            | sm_num`elem`supported_sms -> return ("sm_"++sm_num)
            where supported_sms = ["80", "75", "72", "70"]
          _ -> pSemanticError loc (sm ++ ": unsupported sm version")

pListingElem :: PI [TextSection]
pListingElem = pTextSec <|> pOtherChar
  where -- pOtherLine = pAnyLine >> return [] <* pWhiteSpace
        pTextSec = do
          ts <- pTextSection
          return [ts]

pOtherChar :: PI [TextSection]
pOtherChar = P.anyChar >> pWhiteSpace >> return []

pAnyLine :: PI String
pAnyLine = pLabel "any line" $ do
    pTraceLA $ "pAnyLine.start"
    (pEmptyLine <|> pNonEmptyLine)
  where pEmptyLine = pEndLine >> return ""
        pNonEmptyLine = P.many1 (P.noneOf "\r\n") <* pEndLineOrEOF
        pEndLine = (P.crlf >> return ()) <|> (P.newline >> return ())
        pEndLineOrEOF = pEndLine <|> P.eof


pTextSection :: PI TextSection
pTextSection = pLabel "text section" $ do
  kernel_name <- P.try pTextSectionStart
  is <- pInsts 0
  return (kernel_name, is)

pTextSectionStart :: PI String
pTextSectionStart = pSymbol ".text." >> pIdentifier <* pSymbol ":"


pInsts :: PC -> PI [Inst]
pInsts pc = pEndOfKernel <|> pLabelDef <|> pInstI
  where pLabelDef :: PI [Inst]
        pLabelDef = pWithLoc $ \loc -> do
          id <- ('.':) <$> (P.char '.' >> pIdentifier <* pSymbol ":")
          ldefs <- pisLabelDefinitions <$> pGet
          case id`lookup`ldefs of
            Nothing -> do
              pModify $ \pis ->
                pis {
                  pisLabelDefinitions = (id,pc):pisLabelDefinitions pis
                }
              pInsts pc
            Just _ -> pSemanticError loc "label redefinition"

        pEndOfKernel :: PI [Inst]
        pEndOfKernel = (pLookingAtNewSection <|> P.eof) >> return []
          where pLookingAtNewSection = P.lookAhead (P.try (pKeyword ".section"))

        pInstI :: PI [Inst]
        pInstI = do
          i <- pInst pc
          (i:) <$> pInsts (pc + 16)

