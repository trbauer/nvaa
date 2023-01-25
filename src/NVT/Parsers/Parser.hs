module NVT.Parsers.Parser where

import NVT.Loc
import NVT.Diagnostic


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
import Data.Bits
import Data.Char
import Data.List
import Data.Ord
import Data.Word
import Debug.Trace


-------------------------------------------------------------------------------
type P m u = P.ParsecT String (PSt u) (ExceptT Diagnostic m)
data PSt u =
  PSt {
    pstNextId :: !Int
  , psWarnings :: ![Diagnostic]
  , psUserState :: !u
  }
initPSt :: u -> PSt u
initPSt u = PSt 1 [] u

type PID u a = P Identity u a
type ParseResult u a = Either Diagnostic (a,u,[Diagnostic])

runPID ::
    PID u a ->
    u ->
    FilePath ->
    String ->
    ParseResult u a
runPID pma u file = runIdentity . runP pma u file


runP :: Monad m
    => P m u a
    -> u
    -> FilePath
    -> String
    -> m (ParseResult u a)
runP pma u file inp = body
  where body =
          case runExceptT (P.runParserT pma_with_warnings (initPSt u) file inp) of
            mea -> do
            ea <- mea
            case ea of
              -- semantic error (throwSemanticError)
              Left err -> return $ Left err
              -- parse error
              Right (Left err) -> return $ Left (errToDiag err)
              -- success
              Right (Right (a,u,ws)) -> return (Right (a,u,reverse ws))

        pma_with_warnings = do
          a <- pma
          u <- psUserState <$> P.getState
          ws <- psWarnings <$> P.getState
          -- return (a,map (uncurry Diagnostic) ws)
          return (a,u,ws)

        errToDiag :: P.ParseError -> Diagnostic
        errToDiag err = dCons eloc emsg
          where eloc = sourcePosToLoc (P.errorPos err)
                emsg =
                  drop 1 $ dropWhile (/=';') $
                    concatMap (\c -> if c == '\n' then "; " else [c]) (show err)


sourcePosToLoc :: P.SourcePos -> Loc
sourcePosToLoc sp = lCons (P.sourceName sp) (P.sourceLine sp) (P.sourceColumn sp)

pWarning :: Monad m => Loc -> String -> P m u ()
pWarning loc msg =
  P.modifyState $ \ps -> ps{psWarnings = dCons loc msg:psWarnings ps}

pSemanticError :: (MonadTrans t, Monad m) => Loc -> String -> t (ExceptT Diagnostic m) a
pSemanticError loc msg = pSemanticErrorRethrow $ dCons loc msg
pSemanticErrorRethrow :: (MonadTrans t, Monad m) => Diagnostic -> t (ExceptT Diagnostic m) a
pSemanticErrorRethrow d = lift (throwE d)


pTrace :: Monad m => String -> P m u ()
pTrace = pTraceLAK 0
pTraceLA :: Monad m => String -> P m u ()
pTraceLA = pTraceLAK 8
pTraceLAK :: Monad m => Int -> String -> P m u ()
pTraceLAK k msg = do
  inp <- P.getInput
  loc <- pGetLoc
  let inp_pfx = init (show (take k inp)) ++ sfx
        where sfx
                | length (take (k+1) inp) == k + 1 = "...\""
                | otherwise = "\""
      ctx
        | k <= 0 = ""
        | otherwise = "pTrace(" ++ lFormat loc ++ ": " ++ inp_pfx ++ "): "
  trace (ctx ++ msg) (return ())

pDebug :: (Monad m, Show a) => String -> P m u a -> P m u a
pDebug lbl pa = do
  inp1 <- P.getInput
  pTrace $ ">> " ++ lbl ++ " | " ++ show (take 10 inp1) ++ "  ..."
  a <- pa
  inp2 <- P.getInput
  pTrace $ "<< " ++ lbl ++ " | " ++ show a ++ " | " ++ show (take 10 inp2) ++ "  ..."
  return a

pLabel :: Monad m => String -> P m u a -> P m u a
pLabel = flip P.label

-- pDefineSymbol :: Monad m => Loc -> String -> Int -> P m u ()
-- pDefineSymbol at name off = do
--   ps <- P.getState
--   case find (\(nm,_,_) -> nm == name) (psSymbolDefinitions ps) of
--     Just (nm,_,at_orig) -> do
--       throwSemanticError at ("redefintion of symbol (orginally defined at " ++ fmtLoc at_orig)
--     Nothing -> do
--       P.setState $ ps{psSymbolDefinitions = (name,off,at):psSymbolDefinitions ps}

----------------------------------------------
-- user state
pGet :: Monad m => P m u u
pGet = psUserState <$> P.getState
pGets :: Monad m => (u -> a) -> P m u a
pGets f = f <$> pGet
pModify :: Monad m => (u -> u) -> P m u ()
pModify f = do
  pst <- P.getState
  P.setState pst{psUserState = f (psUserState pst)}
pSet :: Monad m => u -> P m u ()
pSet u = pModify (const u)

----------------------------------------------
-- pMany :: Monad m => P m u a -> P m u [a]
-- pMany = P.many

----------------------------------------------
pGetLoc :: Monad m => P m u Loc
pGetLoc = sourcePosToLoc <$> P.getPosition
pWithLoc :: Monad m => (Loc -> P m u a) -> P m u a
pWithLoc = (pGetLoc >>=)


pWithNextId :: Monad m => (Int -> P m u a) -> P m u a
pWithNextId pa = do
  pst <- P.getState
  let id = pstNextId pst :: Int
  P.setState pst{pstNextId = id + 1}
  pa id


def :: Monad m => P.GenLanguageDef String (PSt u) m
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
tokenParser :: Monad m => P.GenTokenParser String (PSt u) m
tokenParser = P.makeTokenParser def

pWhiteSpace :: Monad m => P m u ()
pWhiteSpace = P.whiteSpace tokenParser

pIdentifier :: Monad m => P m u String
pIdentifier = P.identifier tokenParser

-- an identifier matching this string and not followed by an
-- identifier character
-- pKeyword "let" mismatches "let0" and "let_"
pKeyword :: Monad m => String -> P m u ()
pKeyword s = do
  P.string s
  P.notFollowedBy (P.alphaNum <|> P.oneOf "_")
  pWhiteSpace
  return ()
pTryKeyword :: Monad m => String -> P m u ()
pTryKeyword = P.try . pKeyword

pSymbol :: Monad m => String -> P m u String
pSymbol = P.symbol tokenParser

pSymbol_ :: Monad m => String -> P m u ()
pSymbol_ str = pSymbol str >> return ()

pTrySymbol :: Monad m => String -> P m u String
pTrySymbol = P.try . pSymbol

pFloating :: Monad m => P m u Double
pFloating = P.float tokenParser

-- pInteger :: Monad m => P m u Integer
-- pInteger = P.integer tokenParser

pStringLiteral :: Monad m => P m u String
pStringLiteral = P.stringLiteral tokenParser

pLexeme :: Monad m => P m u a -> P m u a
pLexeme = P.lexeme tokenParser

pImm8 :: Monad m => P m u Word8
pImm8 = pImmA
pImm16 :: Monad m => P m u Word16
pImm16 = pImmA
pImm32 :: Monad m => P m u Word32
pImm32 = pImmA
pImm64 :: Monad m => P m u Word64
pImm64 = pImmA


pImmA :: (Show a, Bounded a, FiniteBits a, Monad m, Integral a, Ord a, Num a) => P m u a
pImmA = do
  a <- pHexImm <|> pBinImm <|> pDecImm
  -- P.notFollowedBy $ P.char '.' >> P.char '#'
  pWhiteSpace
  return a

{-
-- [0-9]+
--
-- This progressively multiplies the old value by 10
-- and adds the new digit (the usual trick), and throws a fit
-- if the new value is smaller (indicating overflow)
pDecImm :: (Monad m, Integral a, Ord a, Num a) => P m u a
pDecImm = pWithLoc $ \loc -> do
    ds <- P.many1 P.digit
    mulAddLoop loc 0 ds
  where mulAddLoop loc v [] = return v
        mulAddLoop loc v (d:ds)
          | n < v     = pSemanticError loc "integer too large" -- overflow
          | otherwise = mulAddLoop loc n ds
          where n = 10*v + fromIntegral (digitToInt d)
-}
pDecImm :: (Show a, Bounded a, FiniteBits a, Monad m, Integral a, Ord a, Num a) => P m u a
pDecImm = pWithLoc $ \loc -> do
  ds <- P.many1 P.digit
  pImmLoopUnsigned 10 loc ds

-- 0x[0-9A-Fa-f]+
pHexImm :: (Show a, Bounded a, FiniteBits a, Monad m, Integral a, Ord a, Num a) => P m u a
pHexImm = pWithLoc $ \loc -> do
  P.try (P.char '0' >> P.oneOf "xX");
  ds <- P.many1 P.hexDigit
  pImmLoopUnsigned 16 loc ds

-- 0b[0-1]+
pBinImm :: (Show a, Bounded a, FiniteBits a, Monad m, Integral a, Ord a, Num a) => P m u a
pBinImm = pWithLoc $ \loc -> do
  P.try (P.char '0' >> P.oneOf "bB");
  ds <- P.many1 (P.oneOf "01")
  pImmLoopUnsigned 2 loc ds


-- For the hex/binary version
pImmLoopUnsigned ::
  (Show a, Bounded a, FiniteBits a, Monad m, Integral a, Ord a, Num a) =>
  a ->
  Loc ->
  [Char] ->
  P m u a
pImmLoopUnsigned r loc = loop 0
  where loop v [] = return v
        loop v0 (d:ds)
          | overflowed = do
            pTrace $ show (v0,d_a,v1)
            pSemanticError loc "integer too large for type"
          | otherwise = loop v1 ds
          where v1 = r*v0 + d_a
                d_a = fromIntegral (digitToInt d) `asTypeOf` v0
                --    r*v + d > maxBound
                -- => v > (maxBound - d)/r
                overflowed =
                    v0 > ((maxBound - d_a)`div`r)
                -- underflowed =
                --    v < 0 && ((minBound - d)`div`r)

{-
-- For the hex/binary version
--
-- This is because the immediates for signed types can fail the
-- overflow test.  E.g. 0xFFFFFFFF on Int32, the last digit
-- goes from small positive to negative; hence we just
-- use a max count here
pImmLoopHexBin ::
  (FiniteBits a, Monad m, Integral a, Ord a, Num a) =>
  a ->
  Loc ->
  [Char] ->
  P m u a
pImmLoopHexBin r loc = loop 0 0
  where loop ix v [] = return v
        loop ix v (d:ds)
          | ix >= max_chars = pSemanticError loc "integer too large for type"
          | otherwise = loop (ix+1) n ds
          where n = r*v + fromIntegral (digitToInt d)

        -- e.g. Int32 allows 32/(8/2) = 8 digits
        --  binary allows 32/(2/2) = 32 digits
        max_chars :: Int
        max_chars = finiteBitSize r`div`bits_per_digit
          where bits_per_digit
                  | r == 16 = 4
                  | r == 8 = 3
                  | r == 2 = 1
--        max_chars = (finiteBitSize r)`div`(fromIntegral r`div`2)
-}

pInt :: Monad m => P m u Int
pInt = pImmA


pOneOf :: Monad m => [(String,a)] -> P m u a
pOneOf = loop . orderByLen
  where loop [] = fail "unexpected token"
        loop ((s,a):sas) = P.try (pSymbol s >> return a) <|> loop sas

orderByLen :: [(String,a)] -> [(String,a)]
orderByLen = reverse . sortBy (comparing (length . fst))

pOneOfNamed :: Monad m => String -> [(String,a)] -> P m u a
pOneOfNamed what = pLabel what . loop . orderByLen
  where loop [] = fail $ "expected " ++ what
        loop ((s,a):sas) = P.try (pSymbol s >> return a) <|> loop sas
pOneOfKeyword :: Monad m => String -> [(String,a)] -> P m u a
pOneOfKeyword what = pLabel what . loop . orderByLen
  where loop []
          | null what = fail "unexpected token"
          | otherwise = fail ("expected " ++ what)
        loop ((s,a):sas) = P.try (pKeyword s >> return a) <|> loop sas

pSymbols :: Monad m => [String] -> P m u String
pSymbols  = pOneOf . map (\s -> (s,s))

-- A sep B sep C
-- A sep B
-- A       sep C
-- A
-- ....


{-
pChain ::
  Monad m =>
  P () ->
  [(a,P a)] ->
  P [a]
pChain pSep = loop []
  where loop ras [] = return (reverse ras)
        loop ras (a,pA) = do
          unless (null ras) $
            pSep

pAnyOrderSubset :: Monad m =>
  (a,P a) ->
  (b,P b) ->
  (c,P c) ->
  P () ->
  P (a,b,c)
pAnyOrderSubset (dft_a,pA) (dft_b,pB) (dft_c,pC) pSep =
  pAXX <|> pBXX <|> pCXX
 where pAXX = do
        a <- pA
        pABX a <|> pABC a dft_b <|> return (a,dft_b,dft_c)
       pABX a = do
        pSep
        b <- pB
        pABC a b <|> pABC a dft_b <|> return (a,dft_b,dft_c)
       pAC a = do
        pSep
        b <- pC
        pABC a b <|> return (a,b,dft_c)
       pABC a b = do
        pSep
        c <- pC
        return (a,b,c)
-}