module NVT.Parsers.NVParser(
    module NVT.Parsers.Parser
  , LabelIndex
  , Unresolved
  --
  , PIResult, PI, PISt(..), pis_init
  --
  , runPI, runPI_WLNO, pSectionOffset, pSetSectionOffset
  , sequenceUnresolved
  --
  , testPI, testPIU, testPIF
  ) where


import NVT.Diagnostic
import NVT.IR
import NVT.Parsers.Parser

import Control.Monad
import Data.Bits
import Data.Int
import Data.Word
import Text.Parsec((<|>),(<?>))
import qualified Text.Parsec           as P



type LabelIndex = [(String,PC)]
type Unresolved a = LabelIndex -> Either Diagnostic a
--
type PIResult a = ParseResult PISt a
   -- Either Diagnostic (a,LabelIndex,[Diagnostic])

type PI a = PID PISt a


pis_init :: PISt
pis_init =
  PISt {
    pisLabelReferences = []
  , pisLabelDefinitions = []
  , pisSectionOffset = 0
  }


data PISt =
  PISt {
    pisLabelReferences :: !LabelIndex
  , pisLabelDefinitions :: !LabelIndex
  , pisSectionOffset :: !Int64
  } deriving Show


pSectionOffset :: PI Int64
pSectionOffset = pisSectionOffset <$> pGet

pSetSectionOffset :: Int64 -> PI ()
pSetSectionOffset off = pModify $ \pis -> pis{pisSectionOffset = off}

-------------------------------------------------------------------------------
sequenceUnresolved :: [Unresolved a] -> Unresolved [a]
sequenceUnresolved as lbl_ix = sequence (map ($lbl_ix) as)
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
  String ->
  PIResult a
runPI pa fp inp = runPI_WLNO pa fp 1 inp

runPI_WLNO ::
  PI a ->
  FilePath ->
  Int ->
  String ->
  PIResult a
runPI_WLNO pa fp lno inp =
    case runPID p1 pis_init fp inp of
      Left err -> Left err
      Right (a,pis,ws) -> Right (a,pis,ws)
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
  case runPI (pa <* P.eof) "<interactive>" inp of
    Left d -> putStrLn $ dFormatWithLines (lines inp) d

    Right (a,pis,_) -> do
      mapM_ print (pisLabelReferences pis)
      putStrLn (fmt a)


-------------------------------------------------------------------------------
{-
parseUnresolvedInsts ::
  LabelIndex ->
  PC ->
  FilePath ->
  Int ->
  String ->
  PIResult (Unresolved [Inst])
parseUnresolvedInsts lbl_ix0 pc fp lno inp =
  runPI (pBlocks pc lbl_ix0) fp lno inp

testBlocks :: String -> IO ()
testBlocks inp = testPIF fmt (pBlocks 0 []) inp
  where fmt :: (Unresolved [Inst],LabelIndex) -> String
        fmt (uis,_) =
          case uis [] of
            Left err -> dFormat err
            Right is -> concatMap (\i -> show i ++ "\n") is

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
-}
