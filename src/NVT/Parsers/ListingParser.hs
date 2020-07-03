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

parseListing :: FilePath -> String -> Either Diagnostic [TextSection]
parseListing fp inp = fst <$> parseListingG fp inp

parseListingG :: FilePath -> String -> Either Diagnostic ([TextSection],LabelIndex)
parseListingG fp inp = do
  case runPI pListing fp inp of
    Left err -> Left err
    Right (ts,pis,_) -> return (ts,pisLabelDefinitions pis)


pListing :: PI [TextSection]
pListing = do
  sm_ver <- pListingHeader
  tss <- sequenceUnresolved . concat <$> P.many pListingElem
  --
  ds <- pisLabelDefinitions <$> pGet
  case tss ds of
    Left err -> pSemanticErrorRethrow err
    Right tss -> return tss

--	.headerflags	@"EF_CUDA_TEXMODE_UNIFIED EF_CUDA_64BIT_ADDRESS EF_CUDA_SM80 EF_CUDA_VIRTUAL_SM(EF_CUDA_SM80)"
pListingHeader :: PI String
pListingHeader = pLabel "listing header (.headerflags ...)" $ do
  pWhiteSpace
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

pListingElem :: PI [Unresolved TextSection]
pListingElem = pTextSec <|> pOtherChar
  where -- pOtherLine = pAnyLine >> return [] <* pWhiteSpace
        pTextSec = do
          ts <- pTextSection
          return [ts]

pOtherChar :: PI [Unresolved TextSection]
pOtherChar = P.anyChar >> pWhiteSpace >> return []

pAnyLine :: PI String
pAnyLine = pLabel "any line" $ do
    pTraceLA $ "pAnyLine.start"
    (pEmptyLine <|> pNonEmptyLine)
  where pEmptyLine = pEndLine >> return ""
        pNonEmptyLine = P.many1 (P.noneOf "\r\n") <* pEndLineOrEOF
        pEndLine = (P.crlf >> return ()) <|> (P.newline >> return ())
        pEndLineOrEOF = pEndLine <|> P.eof


pTextSection :: PI (Unresolved TextSection)
pTextSection = pLabel "text section" $ do
  kernel_name <- P.try pTextSectionStart
  --
  uinsts <- pInsts 0
  --
  let u_insts :: Unresolved [Inst]
      u_insts = sequenceUnresolved uinsts
  --
  return $ \ldefs -> do
      is <- u_insts ldefs
      return (kernel_name, is)

pTextSectionStart :: PI String
pTextSectionStart = pSymbol ".text." >> pIdentifier <* pSymbol ":"


pInsts :: PC -> PI [Unresolved Inst]
pInsts pc = pEndOfKernel <|> pLabelDef <|> pInstI
  where pLabelDef :: PI [Unresolved Inst]
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

        pEndOfKernel :: PI [Unresolved Inst]
        pEndOfKernel = (pLookingAtNewSection <|> P.eof) >> return []
          where pLookingAtNewSection = P.lookAhead (P.try (pKeyword ".section"))

        pInstI :: PI [Unresolved Inst]
        pInstI = do
          ui <- pInst pc
          (ui:) <$> pInsts (pc+16)
