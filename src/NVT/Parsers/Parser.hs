module NVT.Parsers.Parser where


import Text.Parsec((<|>),(<?>))
import qualified Data.Map.Strict       as M
import qualified Text.Parsec           as P
import qualified Text.Parsec.Language  as P
import qualified Text.Parsec.Pos       as P
import qualified Text.Parsec.Token     as P

import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Functor.Identity
import Data.Char
import Data.Word
import Debug.Trace

data Loc =
  Loc {
    lFile :: !FilePath
  , lLine :: !Int
  , lColumn :: !Int
  } deriving (Show,Eq)

locCons :: FilePath -> Int -> Int -> Loc
locCons = Loc

fmtLoc :: Loc -> String
fmtLoc l = maybe_fp ++ show (lLine l) ++ ":" ++ show (lColumn l)
  where maybe_fp = if null (lFile l) then "" else (lFile l++":")

type Diagnostic = (Loc,String)

dCons :: Loc -> String -> Diagnostic
dCons = (,)

fmtDiagnostic :: Diagnostic -> String
fmtDiagnostic (loc,message) = fmtLoc loc ++ ": " ++ message

data ParseOpts =
  ParseOpts {
    poFile :: FilePath
  , poStartOffset :: !Int
  } deriving (Eq,Show)


type P m = P.ParsecT String PSt (ExceptT Diagnostic m)
data PSt =
  PSt {
    pstNextId :: !Int
  , psOffset :: !Int
  , psWarnings :: ![Diagnostic]
  , psSymbolDefinitions :: ![(String,Int,Loc)]
  }
initPSt :: ParseOpts -> PSt
initPSt pos = PSt 1 (poStartOffset pos) []


runP :: Monad m
    => ParseOpts
    -> P m a
    -> String
    -> m (Either Diagnostic (a,[Diagnostic]))
  --  -> m (ParseResult a)
runP pos pma inp = body
  where body = case runExceptT (P.runParserT pma_with_warnings (initPSt pos) (poFile pos) inp) of
                 mea -> do
                  ea <- mea
                  case ea of
                    -- semantic error (throwSemanticError)
                    Left err -> return $ Left err
                    -- parse error
                    Right (Left err) -> return $ Left (errToDiag err)
                    -- success
                    Right (Right (a,ws)) -> return (Right (a,ws))

        pma_with_warnings = do
          a <- pma
          ws <- psWarnings <$> P.getState
          -- return (a,map (uncurry Diagnostic) ws)
          return (a,ws)

        errToDiag :: P.ParseError -> Diagnostic
        errToDiag err = dCons eloc emsg
          where eloc = sourcePosToLoc (P.errorPos err)
                emsg = drop 1 $ dropWhile (/=';') (concatMap (\c -> if c == '\n' then "; " else [c]) (show err))

throwSemanticError :: (MonadTrans t, Monad m) => Loc -> String -> t (ExceptT Diagnostic m) a
throwSemanticError loc msg = throwSemanticErrorDiag $ dCons loc msg
throwSemanticErrorDiag :: (MonadTrans t, Monad m) => Diagnostic -> t (ExceptT Diagnostic m) a
throwSemanticErrorDiag d = lift (throwE d)

sourcePosToLoc :: P.SourcePos -> Loc
sourcePosToLoc sp = locCons (P.sourceName sp) (P.sourceLine sp) (P.sourceColumn sp)

traceP :: String -> P m ()
traceP msg = trace msg (return ())
tracePLA :: Monad m => String -> P m ()
tracePLA = tracePLAK 8
tracePLAK :: Monad m => Int -> String -> P m ()
tracePLAK k msg = do
  inp <- P.getInput
  traceP msg
  traceP ("lookahead: " ++ show (take k inp) ++ " ...")

pDefineSymbol :: Monad m => Loc -> String -> Int -> P m ()
pDefineSymbol at name off = do
  ps <- P.getState
  case find (\(nm,_,_) -> nm == name) (psSymbolDefinitions ps) of
    Just (nm,_,at_orig) -> do
      throwSemanticError at ("redefintion of symbol (orginally defined at " ++ fmtLoc at_orig
    Nothing -> do
      P.setState $ ps{psSymbolDefinitions = (name,off,at):psSymbolDefinitions ps}

-- pResolveSymbol :: Monad m => String -> P m Int
-- pResolveSymbol

pGetLoc :: Monad m => P m Loc
pGetLoc = sourcePosToLoc <$> P.getPosition

pWithNextId :: Monad m => (Int -> P m a) -> P m a
pWithNextId pa = do
  pst <- P.getState
  let id = pstNextId pst :: Int
  P.setState pst{pstNextId = id + 1}
  pa id

pAdvanceOffset :: Monad m => Int -> P m ()
pAdvanceOffset k = do
  ps <- P.getState
  pSetOffset (psOffset ps + k)
pResetOffset :: Monad m => P m ()
pResetOffset = pSetOffset 0
pSetOffset :: Monad m => Int -> P m ()
pSetOffset off = do
  ps <- P.getState
  P.setState $ ps{psOffset = off}
pCurrentPc :: Monad m => P m Int
pCurrentPc = psOffset <$> P.getState

def :: Monad m => P.GenLanguageDef String PSt m
def = P.LanguageDef {
                P.commentStart = "/*"
              , P.commentEnd = "*/"
              , P.commentLine = "//"
              , P.nestedComments = False
              , P.identStart = (P.letter <|> P.char '_')
              , P.identLetter = (P.alphaNum <|> P.char '_')
              , P.opStart = P.oneOf (map head ops) -- P.oneOf "-*/+^%"
              , P.opLetter = P.oneOf ""
              , P.reservedNames = []
              , P.reservedOpNames = ops
              , P.caseSensitive = True
              }
              where ops = ["*", "/", "%", "+", "-", "<<", ">>", "~", "&", "|", "^"]
tokenParser :: Monad m => P.GenTokenParser String PSt m
tokenParser = P.makeTokenParser def

pWhiteSpace :: Monad m => P m ()
pWhiteSpace = P.whiteSpace tokenParser

pLiteral :: Monad m => String -> P m String
pLiteral = P.string

pSymbol :: Monad m => String -> P m String
pSymbol = P.symbol tokenParser

pSymbol_ :: Monad m => String -> P m ()
pSymbol_ str = pSymbol str >> return ()

pFloat :: Monad m => P m Double
pFloat = P.float tokenParser

pInteger :: Monad m => P m Integer
pInteger = P.integer tokenParser

pStringLiteral :: Monad m => P m String
pStringLiteral = P.stringLiteral tokenParser

pLexeme :: Monad m => P m a -> P m a
pLexeme = P.lexeme tokenParser

pImm8 :: Monad m => P m Word8
pImm8 = pImmA
pImm16 :: Monad m => P m Word16
pImm16 = pImmA
pImm32 :: Monad m => P m Word32
pImm32 = pImmA
pImm64 :: Monad m => P m Word64
pImm64 = pImmA

pImmA :: (Monad m, Integral a, Ord a, Num a) => P m a
pImmA = do
  a <- pImmBody
  -- P.notFollowedBy $ P.char '.' >> P.char '#'
  pWhiteSpace
  return a

pImmBody :: (Monad m, Integral a, Ord a, Num a) => P m a
pImmBody = P.try pHexImm <|> P.try pBinImm <|> pDecImm

-- 0x[0-9A-Fa-f]+
pHexImm :: (Monad m, Integral a, Ord a, Num a) => P m a
pHexImm = P.char '0' >> P.oneOf "xX" >> pRadix 16 P.hexDigit
-- 0b[0-1]+
pBinImm :: (Monad m, Integral a, Ord a, Num a) => P m a
pBinImm = P.char '0' >> P.oneOf "bB" >> pRadix 2 (P.oneOf "01")
-- [0-9]+
pDecImm :: (Monad m, Integral a, Ord a, Num a) => P m a
pDecImm = pRadix 10 P.digit
-- generalized helper
pRadix :: (Monad m, Integral a, Ord a, Num a) => a -> P m Char -> P m a
pRadix r pDigit = P.many1 pDigit >>= mulAdd r 0
mulAdd :: (Monad m, Integral a, Ord a, Num a) => a -> a -> [Char] -> P m a
mulAdd r v [] = return v
mulAdd r v (c:cs)
  | n < v     = fail "integer too large" -- overflow
  | otherwise = mulAdd r n cs
  where n = v*r + fromIntegral (digitToInt c)

pInt :: Monad m => P m Int
pInt = pImmA

