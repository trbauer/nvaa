module NVT.Parsers.NVParser(
    module NVT.Parsers.Parser
  , LabelIndex
  --
  , PIResult, PI, PISt(..), pis_init
  --
  , runPI, runPI_WLNO, pSectionOffset, pSetSectionOffset
  --
  , testPI, testPIF
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
testPIF :: (a -> String) -> PI a -> String -> IO ()
testPIF fmt pa inp =
  case runPI (pa <* P.eof) "<interactive>" inp of
    Left d -> putStrLn $ dFormatWithLines (lines inp) d

    Right (a,pis,_) -> do
      unless (null (pisLabelReferences pis)) $ do
        putStrLn $ "=== label refs"
        mapM_ print (pisLabelReferences pis)
      putStrLn (fmt a)
