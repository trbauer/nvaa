{-# LANGUAGE FlexibleContexts #-}
module NVT.Demangle where

import NVT.Diagnostic
import NVT.Floats
import NVT.Loc
import NVT.Parsers.Parser

import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Text.Printf
import Text.Parsec((<|>),(<?>))
import qualified Data.Map.Strict       as M
import qualified Text.Parsec           as P
import qualified Text.Parsec.Language  as P
import qualified Text.Parsec.Pos       as P
import qualified Text.Parsec.Token     as P

-- C.f. https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling
-- https://github.com/gcc-mirror/gcc/blob/master/gcc/cp/mangle.c


-- _Z14pcg32_random_rP19pcg_state_setseq_64
-- struct pcg_state_setseq_64 {    // Internals are *Private*.
--    uint64_t state;             // RNG state.  All values are possible.
--    uint64_t inc;               // Controls which RNG sequence (stream) is
-- };
-- typedef struct pcg_state_setseq_64 pcg32_random_t;
-- pcg32_random_r(pcg_state_setseq_64*):

-- _Z5add64PxPKxx
-- add64(long long*, long long const*, long long)

-- void f (void (A::*)() const &) {}
-- produces the mangled name "_Z1fM1AKFvvRE".

--    template<class T> void f(T) {}
--    template void f(int);
--      // Mangled as "_Z1fIiEvT_".

-- template <char const *str>
-- struct X {const char *GetString() const { return str; } };
--
-- X<&global_string>::GetString() const
-- _ZNK1XIXadL_Z13global_stringEEE9GetStringEv

-- double fun<&param>():
-- template <double* pVar> double fun() {return *pVar;}
-- _Z3funIXadL_ZL5paramEEEdv

--
-- template <typename T, T* pVar>T fun() {return *pVar;}
--
-- call    _Z3funIdXadL_ZL6paramDEEET_v
--   double valueD = fun<double, &paramD>();
-- double fun<double, &paramD>()
--
-- call    _Z3funIfXadL_ZL6paramFEEET_v
--   float valueF = fun<float, &paramF>();
-- float fun<float, &paramF>()

--  "A<char, float>" is encoded as "1AIcfE"
-- "A<T2>::X" ... N1AIT0_E1XE
-- T2 is the second template parameter

-- extern "C" bool IsEmpty(char *); // (un)mangled as IsEmpty
-- template<void (&)(char *)> struct CB;
-- CB<IsEmpty> is mangled as "2CBIL_Z7IsEmptyEE"

-- <decltype> =
--          void g(int);
--          template<class T> auto f(T p)->decltype(g(p));
-- dpDeclType "DTcl1gfp_E"
--
-- "_Z13process_pair4P4PairIS_IiiES0_E" ==>
--   process_pair4(Pair<Pair<int, int>, Pair<int, int> >*):
--
--   template <typename T, typename U> struct Pair {T t; U u;};
--   using Pair2 = Pair<int, int>;
--   using Pair4 = Pair<Pair2, Pair2>;
--   void process_pair4(Pair4* p) { }
--
-- "_Z4funcP4PairIiiE"
-- "_Z13process_pair2P4PairIiiE" ==>
--   process_pair2(Pair<int, int>*, Pair<int, int>*)
--     _Z prefix
--
-- "_Z13process_pair2P4PairIiiES1_" ==>
--   process_pair4(Pair<Pair<int, int>, Pair<int, int> >*)
--
-- "_Z13process_pair2P4PairIiiEfS1_" ==>
--   process_pair2(Pair<int, int>*, float, Pair<int, int>*)
--
-- "_Z13process_pair8P4PairIS_IS_IiiES0_ES1_E" ==>
--   process_pair8(Pair<Pair<Pair<int, int>, Pair<int, int> >, Pair<Pair<int, int>, Pair<int, int> > >*):
--   void process_pair8(Pair8* p) { }
demangle :: String -> Either String String
demangle inp =
  case runPID dpMangledName dpst "" inp of
    Left d -> Left (dMessage d)
    Right (a,_,_) -> Right a

testDemangle :: String -> IO ()
testDemangle = dpTest dpMangledName

runTests :: IO ()
runTests = run
  where run = do
          putStrLn $ "=========== TESTS ======"
          forM_ tests $ \(dpa,inp,res) -> do
            putStr $ printf "  %-48s" inp
            case dpTestRet dpa inp of
              Left e -> do
                putStrLn ": error"
                putStrLn e
              Right a
                | a /= res -> putStrLn $ ": mismatched: got " ++ a ++  " (instead of"  ++ res ++ ")"
                | otherwise -> putStrLn "success"

        tests :: [(DP String,String,String)]
        tests =
            [
              t dpName "3fun" "fun"
            , t dpMangledName "_Z6helperv" "helper()"
            ]
          where t = (,,)

-- _Z5add64PxPKxx
-- add64(long long*, long long const*, long long)

--------------------------------------------------------------------------------
-- INTERACTIVE FUNCTIONS
--------------------------------------------------------------------------------

-- demangle parser type
type DP a = PID DPSt a

-- (substs,templates)
data DPSt =
  DPSt {
    dpstSubstitutions :: ![(String,Int)]
  , dpstTemplateParameters :: ![(String,Int)]
  , dpstDepth :: !Int
  , dpstUntraced :: !Bool
  } deriving Show

dpst :: DPSt
dpst = DPSt [] [] 0 False

dpTest :: Show a => DP a -> String -> IO ()
dpTest dpa inp = do
  let dpaw = do {a <-dpa; inp <- P.getInput; return (a,inp)}
  case runPID dpaw dpst "" inp of
    Left d -> putStrLn (dFormatWithLines [inp] d)
    Right ((a,inp),_,_) -> do
      putStrLn $ show a
      unless (null inp) $
        putStrLn $ "with suffix: " ++ show inp
dpTestDemangle :: String -> IO ()
dpTestDemangle inp =
  case runPID dpMangledName dpst "" inp of
    Left d -> putStrLn (dFormatWithLines [inp] d)
    Right (a,_,_) -> putStrLn $ show a

dpTestRet :: Show a => DP a -> String -> Either String a
dpTestRet dpa inp =
  case runPID dpa dpst "" inp of
    Left d -> Left $ dFormatWithLines [inp] d
    Right (a,_,_) -> Right a

--------------------------------------------------------------------------------
-- SHARED
--------------------------------------------------------------------------------

dpInt :: DP Int
dpInt = dpLabel "number" $ read <$> P.many1 P.digit

forS :: String -> DP a -> (String,DP a)
forS = (,)
forC :: Char -> DP a -> (String,DP a)
forC c = forS [c]
strS :: String -> a -> (String,DP a)
strS s v = (s,return v)
strC :: Char -> a -> (String,DP a)
strC c = strS [c]

dpParseFromTable :: String -> [(String,DP a)] -> DP a
dpParseFromTable what table =
  dpParseFromSortedTable what $ sortOn (Down . length . fst) table
dpParseFromSortedTable :: String -> [(String,DP a)] -> DP a
dpParseFromSortedTable what = loop
  where loop [] = fail $ "expected " ++ what ++ " type"
        loop ((str,dpa):tbl) =
            (P.try (P.string str) >> dpa) <|> loop tbl

dpTryString :: String -> DP ()
dpTryString s = P.try (P.string s) >> return ()

dpTrace :: String -> DP ()
dpTrace = pTraceLAK 16

dp_trace :: Bool
dp_trace = True

dpUtracedLookahead :: DP a -> DP Bool
dpUtracedLookahead dpa = do
  pModify $ \dpst -> dpst{dpstUntraced = True}
  success <- P.option False (P.try (P.lookAhead dpa) >> return True)
  pModify $ \dpst -> dpst{dpstUntraced = False}
  return success

dpLabelTrace :: Show a => String -> DP a -> DP a
dpLabelTrace lbl dpa = do
  untraced <- pGets dpstUntraced
  if untraced then dpa
    else do
      let trace = dpTrace
      depth <- pGets dpstDepth
      let indent = replicate (2 * depth) ' '
      inp <- P.getInput
      let k = 16
      let ellipsis = if length (take (k + 1) inp) < k + 1 then "" else "..."
      pTrace $ printf "%-80s" (indent ++ " ==> " ++ lbl) ++
        " [" ++ take k inp ++ ellipsis ++ "]"
      success <- dpUtracedLookahead dpa
      --
      -- unless success $
      --   pTrace $ printf "%-80s" (indent ++ " <XXXX " ++ lbl)

      pModify $ \dpst -> dpst{dpstDepth = dpstDepth dpst + 1}
      a <- pLabel lbl dpa
      pModify $ \dpst -> dpst{dpstDepth = dpstDepth dpst - 1}
      pTrace $ printf "%-80s" (indent ++ " <== " ++ lbl)
      return a

dpLabel :: Show a => String -> DP a -> DP a
dpLabel
  | not dp_trace = pLabel
  | otherwise = dpLabelTrace

dpLookup :: String -> (DPSt -> [(String,Int)]) -> Loc -> Int -> DP String
dpLookup what app loc idx = do
  tbl <- pGets app
  when (idx >= length tbl) $
    pSemanticError loc $ what ++ " out of bounds"
  return $ fst (tbl !! idx)

-- used in <substitution>
dpLookupSubstitution :: Loc -> Int -> DP String
dpLookupSubstitution = dpLookup "substitution" dpstSubstitutions

-- used in <template-param>
dpLookupTemplateParameter :: Loc -> Int -> DP String
dpLookupTemplateParameter = dpLookup "template parameter" dpstTemplateParameters

dpSubstitutionDef :: DP String -> DP String
dpSubstitutionDef dps = pWithLoc $ \loc -> do
  let col = lColumn loc
  dpst <- pGet
  -- reserve a space for the symbol
  let our_ix = length (dpstSubstitutions dpst)
  pSet (dpst{dpstSubstitutions = dpstSubstitutions dpst ++ [("###ERR:dpSubstitutionDef",col)]})
  --
  -- decode it
  sym <- dps
  let upd ix ((_,_):syms)
        | ix == our_ix = (sym,col):syms
        | otherwise = upd (ix + 1) syms
  pModify $ \dpst -> dpst{dpstSubstitutions = upd our_ix (dpstSubstitutions dpst)}
  --
  return sym


--------------------------------------------------------------------------------
-- Top Level
--------------------------------------------------------------------------------

-- <mangled-name>
--   ::= _Z <encoding>
--   ::= _Z <encoding> . <vendor-specific suffix>
dpMangledName :: DP String
dpMangledName = do
  P.string "_Z" <?> "demangle prefix _Z"
  e <- dpEncoding
  vendor_sfx <- P.option "" $ do
    P.char '.'
    ("."++) <$> P.many P.anyChar
  P.eof
  return (e ++ vendor_sfx)

-- 5.1.2
-- <encoding>
--   ::= <function name> <bare-function-type>
--	 ::= <data name>
--	 ::= <special-name>
dpEncoding :: DP String
dpEncoding = dpLabel "<encoding>" $ do
  P.try dpSpecialName <|> do
      -- data or function
      nm <- dpName
      bft <- P.option "" dpBareFunctionType
      return (nm ++ bft)

-- <bare-function-type> ::= <signature type>+
--	# types are possible return type, then parameter types
dpBareFunctionType :: DP String
dpBareFunctionType = dpLabel "<bare-function-type>" $ do
  sigs <- P.many1 dpType
  return $
    case sigs of
      ["void"] ->  "()"
      _ -> "(" ++ intercalate ", " sigs ++ ")"


--------------------------------------------------------------------------------
-- NAMES
--------------------------------------------------------------------------------
--
-- <name>
--   ::= <nested-name>
--	 ::= <unscoped-name>
--	 ::= <unscoped-template-name> <template-args>
--	 ::= <local-name>	# See Scope Encoding below
dpName :: DP String
dpName = dpLabelTrace "<name>" $
        dpNestedName
    <|> dpUnscopedTemplateName_TemplateArgs
    <|> dpUnscopedName
    <|> dpLocalName
  where dpUnscopedTemplateName_TemplateArgs = do
          tm <- dpUnscopedTemplateName -- ==> dpUnscopedName <|> subst
          dpTrace $ "HERE: "++show tm
          ta <- dpTemplateArgs
          return $ tm ++ ta


-- <nested-name>
--   ::= N [<CV-qualifiers>] [<ref-qualifier>] <prefix> <unqualified-name> E
--   ::= N [<CV-qualifiers>] [<ref-qualifier>] <template-prefix> <template-args> E
dpNestedName :: DP String
dpNestedName = dpLabel "<nested-name>" $ do
  P.char 'N'
  fail "dpNestedName: not handled yet"
  cv <- dpCvQualifiers
  ref <- P.option "" dpRefQualifier
  --
  nn <- dpPrefix_UnqualifiedName <|> dpTemplatePrefix_TemplateArgs
  P.char 'E'
  return nn

dpPrefix_UnqualifiedName :: DP String
dpPrefix_UnqualifiedName = do
  pfx <- dpPrefix
  uqn <- dpUnqualifiedName
  return (pfx ++ "::" ++ uqn)

-- <template-prefix> <template-args>
dpTemplatePrefix_TemplateArgs :: DP String
dpTemplatePrefix_TemplateArgs = do
  pfx <- dpTemplatePrefix
  ta <- dpTemplateArgs
  return (pfx ++ "<" ++ ta ++ ">")

dpTemplateArgs :: DP String
dpTemplateArgs = dpLabel "<template-args>" $ do
  P.char 'I'
  as <- P.many1 dpTemplateArg
  P.char 'E'
  return $ "<" ++ intercalate "," as ++ ">"

dpTemplateArg :: DP String
dpTemplateArg = dpLabel "<template-arg>" $
    dpType <|> dpTmplExpr <|> dpExprPrimary <|> dpTmplArgPack
  where dpTmplExpr = do
          P.char 'X'
          e <- dpExpression
          P.char 'E'
          return e

        dpTmplArgPack = do
          P.char 'J'
          ta <- intercalate "," . map (++"...") <$> P.many dpTemplateArg
          P.char 'E'
          return $ ta ++ "..."

--
-- <prefix>
--   ::=          <unqualified-name>    # global class or namespace
--   ::= <prefix> <unqualified-name>    # nested class or namespace
--   ::=          <template-prefix> <template-args>  # class template specialization
--   ::=          <template-param>      # template type parameter
--   ::=          <decltype>            # decltype qualifier
--   ::= <prefix> <data-member-prefix>  # initializer of a data member
--   ::=          <substitution>
--
-- left-recursion removal:
--   X => X a | b
-- is  (b a*)
--  X => ...
--        X a
--      X a a
--    X a a a
--    b a a a
--
-- Problem is that we need to define substitution
dpPrefix :: DP String
dpPrefix = dpLabel "<prefix>" $ do
    pfx <- pNonRecursive
    sfx <- P.many (dpUnqualifiedName <|> dpDataMemberPrefix)
    return $ pfx ++ concat sfx
  where pNonRecursive :: DP String
        pNonRecursive = dpSubstitutionDef pNonSubst <|> dpSubstitution
          where pNonSubst =
                      dpUnqualifiedName
                  <|> dpTemplatePrefix_TemplateArgs
                  <|> dpTemplateParam
                  <|> dpDeclType

-- <template-prefix>
--   ::= <template unqualified-name>           # global template
--   ::= <prefix> <template unqualified-name>  # nested template
--   ::= <template-param>                      # template template parameter
--   ::= <substitution>
dpTemplatePrefix :: DP String
dpTemplatePrefix = dpLabel "<template-prefix>" $
      dpUnqualifiedName -- global template
  <|> dpPrefix_UnqualifiedName -- nested template
  <|> dpTemplateParam

-- <unqualified-name>
--   ::= <operator-name> [<abi-tags>]
--   ::= <ctor-dtor-name>
--   ::= <source-name>
--   ::= <unnamed-type-name>
--   ::= DC <source-name>+ E      # structured binding declaration
dpUnqualifiedName :: DP String
dpUnqualifiedName = dpLabel "<unqualified-name>" $
        dpOperatorName_AbiTags
    <|> dpCtrDtorName
    <|> dpSourceName
    <|> dpUnnamedTypeName
    <|> dpStructuredBindingDecl
  where pOperatorName_AbiTags = liftM2 (++) dpOperatorName dpAbiTags
        -- ::= DC <source-name>+ E      # structured binding declaration
        pStructuredBindingDecl = do
          P.try $ P.string "DC"
          nms <- P.many1 dpSourceName
          P.char 'E'
          return $ intercalate "?" nms

        -- ::= DC <source-name>+ E      # structured binding declaration
        dpStructuredBindingDecl :: DP String
        dpStructuredBindingDecl = do
          P.string "DC"
          nms <- P.many1 dpSourceName
          P.char 'E'
          return $ "<structured binding " ++ intercalate "," nms ++ ">"

        dpOperatorName_AbiTags :: DP String
        dpOperatorName_AbiTags = do
          op <- dpOperatorName
          abi <- P.option "" dpAbiTags
          return $ op ++ abi

dpCtrDtorName :: DP String
dpCtrDtorName = dpLabel "<ctor-dtor-name>" $ dpParseFromTable "<ctor-dtor-name>" [
    -- not sure how to decode these
    forS "C1" $ return "complete object constructor"
  , forS "C2" $ return "base object constructor"
  , forS "C3" $ return "complete object allocating constructor"
  , forS "CI1" $ ("complete object inheriting constructor "++) <$> dpType
  , forS "CI2" $ ("base object inheriting constructor "++) <$> dpType
  , forS "D0" $ return "deleting destructor"
  , forS "D1" $ return "complete object destructor"
  , forS "D2" $ return "base object destructor"
  ]

-- <data-member-prefix> ::= <member source-name> [<template-args>] M
dpDataMemberPrefix :: DP String
dpDataMemberPrefix = dpLabel "<data-member-prefix>" $ do
  member <- dpSourceName
  as <- P.option "" dpTemplateArgs
  P.char 'M'
  return $ member ++ as

dpUnnamedTypeName :: DP String
dpUnnamedTypeName = dpLabel "<unnamed-type-name>" $ do
  P.string "Ut"
  n <- dpInt
  return $ "unnamed-type:" ++ show n
  --
  fail "dpUnnamedTypeName: not understood yet"

dpSpecialName :: DP String
dpSpecialName = dpLabel "<special-name>" $ do
  P.char 'T'
  c <- P.oneOf "VTIS"
  return $
    case c of
      'V' -> "[virtual table]"
      'T' -> "[VTT structure]"
      'I' -> "[typeinfo stucture]"
      'S' -> "[typeinfo name]"

--    <abi-tags> ::= <abi-tag> [<abi-tags>]
--    <abi-tag> ::= B <source-name>
-- For example:
--  struct [[gnu::abi_tag ("foo","bar")]] A { }; // mangles as 1AB3barB3foo
-- c++filt gives "A[abi:bar][abi:foo]"
dpAbiTags :: DP String
dpAbiTags = dpLabel "<abi-tags>" $ concat <$> do
  P.many $ do
    P.char 'B'
    (\s -> "[abi:" ++ s ++ "]") <$> dpSourceName

dpOperatorName :: DP String
dpOperatorName = dpLabel "<operator-name>" $ ("operator "++) <$>
  dpParseFromTable "<operator-name>" [
    forS "nw" $ return "new"
  , forS "da" $ return "new[]"
  , forS "dl" $ return "delete"
  , forS "da" $ return "delete[]"
  , forS "aw" $ return "co_await"
  , forS "ps" $ return "+ (unary)"
  , forS "ng" $ return "- (unary)"
  , forS "ad" $ return "& (unary)"
  , forS "de" $ return "* (unary)"
  , forS "co" $ return "~"
  , forS "pl" $ return "+"
  , forS "mi" $ return "-"
  , forS "ml" $ return "*"
  , forS "dv" $ return "/"
  , forS "rm" $ return "%"
  , forS "an" $ return "&"
  , forS "or" $ return "|"
  , forS "eo" $ return "^"
  , forS "aS" $ return "="
  , forS "pL" $ return "+="
  , forS "mI" $ return "-="
  , forS "mL" $ return "*="
  , forS "dV" $ return "/="
  , forS "rM" $ return "%="
  , forS "aN" $ return "&="
  , forS "oR" $ return "|="
  , forS "eO" $ return "^="
  , forS "ls" $ return "<<"
  , forS "rs" $ return ">>"
  , forS "lS" $ return "<<="
  , forS "rS" $ return ">>="
  , forS "eq" $ return "=="
  , forS "ne" $ return "!="
  , forS "lt" $ return "<"
  , forS "gt" $ return ">"
  , forS "le" $ return "<="
  , forS "ge" $ return ">="
  , forS "ss" $ return "<=>"
  , forS "nt" $ return "!"
  , forS "aa" $ return "&&"
  , forS "oo" $ return "||"
  , forS "pp" $ return "++" -- postfix in expression context
  , forS "mm" $ return "--" -- postfix in expression context
  , forS "cm" $ return ","
  , forS "pm" $ return "->*"
  , forS "pt" $ return "->"
  , forS "cl" $ return "()"
  , forS "ix" $ return "[]"
  , forS "qu" $ return "?"
  , forS "cv" $ do -- conversion/cast
      t <- dpType
      return $ "(" ++ t ++ ")"
  , forS "li" $ ("\"\" " ++) <$> dpSourceName  -- operator ""
  -- ::= v <digit> <source-name>	# vendor extended operator
  -- forS "v" $ vendor extended
  ]

-- <unscoped-name>
--   ::=    <unqualified-name>
--   ::= St <unqualified-name>   # ::std::
dpUnscopedName :: DP String
dpUnscopedName = dpLabel "<unscoped-name>" $ do
    dpUnqual <|> dpStUnqual
  where dpUnqual =
          dpUnqualifiedName
        dpStUnqual = do
          dpTryString "St"
          (++"std::") <$> dpUnqualifiedName -- trimmed prefixing ::

-- <unscoped-template-name>
--   ::= <unscoped-name>
--	 ::= <substitution>
dpUnscopedTemplateName :: DP String
dpUnscopedTemplateName = dpLabel "<unscoped-template-name>" $ do
  dpSubstitutionDef dpUnscopedName <|> dpSubstitution

-- <local-name>
--   ::= Z <function encoding> E <entity name> [<discriminator>]
--   ::= Z <function encoding> E s             [<discriminator>]
dpLocalName :: DP String
dpLocalName = dpLabel "<local-name>" $ do
  fail "dlLocalName: need to understand these"
  P.char 'Z'
  func <- dpEncoding
  P.char 'E'
  let pSfx0 = P.char 's' >> return "[s?]"
      pSfx1 = dpName
  sfx <- pSfx0 <|> pSfx1
  disc <- dpDiscriminator
  return $ func ++ sfx ++ disc

-- <discriminator>
--   ::= _ <non-negative number>      # when number < 10
--   ::= __ <non-negative number> _   # when number >= 10
dpDiscriminator :: DP String
dpDiscriminator = dpLabel "<discriminator>" $ do
  P.char '_'
  let pLt10 = dpNumber
      pGe10 = P.char '_' >> dpNumber
  pLt10 <|> pGe10


-- <source-name> ::= <positive length number> <identifier>
--    <identifier> ::= <unqualified source code identifier>
dpSourceName :: DP String
dpSourceName =  dpLabel "<source-name>" $ do
  n <- dpInt
  sequence $ replicate n P.anyChar

dpUnresolvedName :: DP String
dpUnresolvedName = do
  scope <- P.option id $ dpTryString "gs" >> return ("::"++)
  dpBaseUnresolvedName

dpBaseUnresolvedName :: DP String
dpBaseUnresolvedName =
    dpSimpleId <|> dpOnX <|> dpDnDestructor
  where dpOnX = do
          dpTryString "on"
          op <- dpOperatorName
          tas <- P.option "" $ (\a -> "<" ++ a ++ ">") <$> dpTemplateArgs
          return $ op ++ tas

dpDnDestructor :: DP String
dpDnDestructor = dpUnresolvedType <|> dpSimpleId

dpUnresolvedType :: DP String
dpUnresolvedType =
  dpSubstitutionDef (dpTemplateParam_TemplateArgs <|> dpDeclType) <|> dpSubstitution

dpSimpleId :: DP String
dpSimpleId = do
  sn <- dpSourceName
  tas <- P.option "" $ (\a -> "<" ++ a ++ ">") <$> dpTemplateArgs
  return $ sn ++ tas

dpTemplateParam_TemplateArgs :: DP String
dpTemplateParam_TemplateArgs = do
  tp <- dpTemplateParam
  tas <- P.option "" $ dpTemplateArgs
  return $ tp ++ tas


--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

-- Has indirect left recursion:
--
--   dpType => dpQualifiedType =>
--       dpQualifiers dpType =>
--       dpExtendedQualifier* dpCvQualifiers dpType =>
--       dpExtendedQualifier* [r] [V] [K] dpType =>
--       dpType
--
-- Break this by splitting <type> set between those that include
-- optional qualifier prefixes and those that don't
dpType :: DP String
dpType = dpLabel "<type>" $ dpQualifiedType <|> dpTypeNotQualified
dpTypeNotQualified :: DP String
dpTypeNotQualified = dpLabel "<type>" $
    dpSubstitutionDef dpNonSubst <|> dpSubstitution
  where dpNonSubst =
          dpBuiltinType <|>
            dpFunctionType <|>
            dpClassEnumType <|>
            dpArrayType <|>
            dpPointerToMemberType <|>
            dpTemplateParam <|>
            dpTemplateTemplateParam <|>
            dpDeclType <|>
            dpPointerType <|>
            dpModif "R" (++"&") dpType <|>
            dpModif "O" (++"&&") dpType <|>
            dpModif "C" (++"_Complex") dpType <|>
            dpModif "G" (++"_Imaginary") dpType


dpPointerType :: DP String
dpPointerType = dpModif "P" (++"*") dpType

dpModif :: String -> (String -> String) -> DP String -> DP String
dpModif pfx func pSfx = P.try $ do
  P.string pfx
  func <$> pSfx

----------------------------------------------

dpBuiltinType :: DP String
dpBuiltinType =
  dpLabel "<builtin-type>" $ dpParseFromTable "<builtin-type>"
    [
      strC 'a' "signed char"
    , strC 'b' "bool"
    , strC 'c' "char"
    , strC 'd' "double"
    , strC 'e' "long double"
    , strC 'f' "float"
    , strC 'g' "__float128"
    , strC 'h' "unsigned char"
    , strC 'i' "int"
    , strC 'j' "unsigned int"
    , strC 'l' "long"
    , strC 'm' "unsigned long"
    , strC 'n' "_int128"
    , strC 'o' "unsigned _int128"
    , strC 's' "short"
    , strC 't' "unsigned short"
    , strC 'v' "void"
    , strC 'w' "wchar_t"
    , strC 'x' "long long"
    , strC 'y' "unsigned long long"
    , strC 'z' "..." -- ellipsis
    --
    -- ::= Dd # IEEE 754r decimal floating point (64 bits)
    -- ::= De # IEEE 754r decimal floating point (128 bits)
    -- ::= Df # IEEE 754r decimal floating point (32 bits)
    -- ::= Dh # IEEE 754r half-precision floating point (16 bits)
    -- ::= DF <number> _ # ISO/IEC TS 18661 binary floating point type _FloatN (N bits)
    , forS "Dd" $ fail "dpBuiltinType: IEEE 754r decimal types not supported"
    , forS "De" $ fail "dpBuiltinType: IEEE 754r decimal types not supported"
    , forS "Df" $ fail "dpBuiltinType: IEEE 754r decimal types not supported"
    , forS "Dh" $ fail "dpBuiltinType: IEEE 754r decimal types not supported"
    , forS "DF" $ fail "dpBuiltinType: IEEE 754r decimal types not supported"
    --
    , strS "Di" "char32_t"
    , strS "Ds" "char16_t"
    , strS "Du" "char8_t"
    , strS "Da" "auto"
    , strS "Dc" "decltype(auto)"
    , strS "Dn" "std::nullptr_t" -- or decltype(nullptr)
    , forC 'u' $ fail "dpBuiltinType: vendor extended types not supported"
    -- ::= u <source-name> [<template-args>] # vendor extended type
    ]

--  <qualified-type> ::= <qualifiers> <type>
dpQualifiedType :: DP String
dpQualifiedType = dpLabel "<qualified-type>" $ do
  -- choosing to do this in any order
  quals <- dpQualifiers
  (quals ++) <$> dpTypeNotQualified -- break left recursion

-- <qualifiers> ::= <extended-qualifier>* <CV-qualifiers>
dpQualifiers :: DP String
dpQualifiers =  dpLabel "<qualifiers>" $ do
  ext_quals <- concatMap (++ " ") <$> P.many dpExtendedQualifier
  cv_quals <- dpCvQualifiers
  return $ ext_quals ++ cv_quals

--
--  <extended-qualifier> ::= U <source-name> [<template-args>] # vendor extended type qualifier
dpExtendedQualifier :: DP String
dpExtendedQualifier = dpLabel "<extended-qualifier>" $ do
  P.char 'U'
  nm <- dpSourceName
  (nm ++) <$> dpTemplateArgs

--  <CV-qualifiers> ::= [r] [V] [K] 	  # restrict (C99), volatile, const
dpCvQualifiers :: DP String
dpCvQualifiers = dpLabel "<CV-qualifiers>" $ do
  let dpCvQualifier = do
        -- should be ordered, but we'll be sloppy
        c <- P.oneOf "rVK"
        return $
          case c of
            'r' -> "restrict "
            'V' -> "volatile "
            'K' -> "const "
  concat <$> P.many dpCvQualifier

dpRefQualifier :: DP String
dpRefQualifier = dpLabel "<ref-qualifier>" $ do
  c <- P.oneOf "RO"
  return $
    case c of
      'R' -> "&"
      'O' -> "&&"

-- <class-enum-type>
--   ::=    <name>  # non-dependent type name, dependent type name, or dependent typename-specifier
--   ::= Ts <name>  # dependent elaborated type specifier using 'struct' or 'class'
--   ::= Tu <name>  # dependent elaborated type specifier using 'union'
--   ::= Te <name>  # dependent elaborated type specifier using 'enum'
dpClassEnumType :: DP String
dpClassEnumType = dpLabel "<class-enum-type>" $ do
  let pTy pfx what = (what ++) <$> (dpTryString pfx >> dpName)
  pTy "" "" <|> pTy "Ts" "struct " <|> pTy "Tu" "union " <|> pTy "Te" "enum "

-- <array-type>
--   ::= A <positive-dimension number> _ <element type>
--	 ::= A [<dimension expression>]    _ <element type>
--
-- "_Z3fooILi2EEvRAplT_Li1E_i" =>
--     template<int I> void foo (int (&)[I + 1]) { }
--     template void foo<2> (int (&)[3]);
dpArrayType :: DP String
dpArrayType = dpLabel "<array-type>" $ do
  P.char 'A'
  let pArrNum = dpNumber
      pArrExpr = P.option "" dpExpression
  dim_expr <- pArrNum <|> pArrExpr
  P.char '_'
  elem <- dpType
  return $ elem ++ "[" ++ dim_expr ++ "]"


-- <pointer-to-member-type> ::= M <class type> <member type>
-- "_Z1fM1AKFvvRE" =>
--   void f (void (A::*)() const &) {}
dpPointerToMemberType :: DP String
dpPointerToMemberType = dpLabel "<pointer-to-member-type>" $ do
  P.char 'M'
  cls <- dpType
  mem <- dpType
  return (cls ++ "::" ++ mem)

-- <template-param>
--   ::= T_	# first template parameter
--   ::= T <parameter-2 non-negative number> _
--
-- "_Z1fIiEvT_" =>
--    template<class T> void f(T) {}
--    template void f(int);
dpTemplateParam :: DP String
dpTemplateParam = dpLabel "<template-param>" $ pWithLoc $ \loc ->
    P.char 'T' >> (t1 loc <|> tN loc)
  where t1 loc = do
          P.char '_'
          dpLookupTemplateParameter loc 0

        tN loc = do
          n <- dpInt <* P.char '_'
          dpLookupTemplateParameter loc (n - 1)


-- <template-template-param>
--   ::= <template-param>
--   ::= <substitution>
dpTemplateTemplateParam :: DP String
dpTemplateTemplateParam = dpLabel "<template-template-param>" $
  dpSubstitutionDef dpTemplateParam <|> dpSubstitution

-- <decltype>
--   ::= Dt <expression> E  # decltype of an id-expression or class member access (C++11)
--   ::= DT <expression> E  # decltype of an expression (C++11)
dpDeclType :: DP String
dpDeclType = dpLabel "<decltype>" $ do
  P.try $ P.char 'D' >> P.oneOf "tT"
  e <- dpExpression
  P.char 'E'
  return $"decltype(" ++ e ++ ")"

-- <substitution>
--   ::= S <seq-id> _
--   ::= S_
--
-- "_Z1fPFvvEM1SFvvE" =>
--   typedef void T();
--   struct S {};
--   void f(T*, T (S::*)) {}
--
-- "_ZSt5state"      => ::std::state
-- "_ZNSt3_In4wardE" => ::std::_In::ward
--
-- "_ZN1N1TIiiE2mfES0_IddE": Ret? N::T<int, int>::mf(N::T<double, double>)
-- since the substitutions generated for this name are:
--    "S_" == N (qualifier is less recent than qualified entity)
--    "S0_" == N::T (template-id comes before template)
-- 	(int is builtin, and isn't considered)
--    "S1_" == N::T<int, int>
--    "S2_" == N::T<double, double>
--
dpSubstitution :: DP String
dpSubstitution = dpLabel "<substitution>" $ pWithLoc $ \loc -> do
    P.char 'S'
    pSubSeq loc <|> pSubConst
  where pSubSeq :: Loc -> DP String
        pSubSeq loc = do
          P.char '_'
          sub <- P.option 0 dpSeqId
          dpLookupSubstitution loc sub

        pSubConst :: DP String
        pSubConst =
          dpParseFromTable "<substitution-built-in>" [
            -- stripped prefixing :: (add back, if needed)
            strC 't' "std::"
          , strC 'a' "std::allocator"
          , strC 'b' "std::basic_string"
          -- , strC 's' "::std::basic_string< char, ::std::char_traits<char>, ::std::allocator<char> >"
          , strC 's' "std::string"
          , strC 'i' "std::istream" -- "::std::basic_istream<char, std::char_traits<char> >"
          , strC 'o' "std::ostream" -- "::std::basic_ostream<char, std::char_traits<char> >"
          , strC 'd' "std::iostream" -- "::std::basic_iostream<char, std::char_traits<char> >"
          ]


-- <seq-id> ::= <0-9A-Z>+
dpSeqId :: DP Int
dpSeqId = dpLabel "<seq-id>" $ do
  sq <- P.many1 $ P.satisfy (\c -> c >= '0' && c <= '9' && c >= 'A' && c <= 'Z')
  let accChr n c = 10 * n + d
        where d
                | c <= '9' && c >= '0' = ord c - ord '0'
                | otherwise = 10 + ord c - ord 'A'
  return $ foldl' accChr 0 sq


-- A transaction-safe function type is encoded with a "Dx" before the "F". This affects only type mangling; a transaction-safe function has the same mangling as a non-transaction-safe function.
--
-- A "Y" prefix for the bare function type encodes extern "C" in implementations which distinguish between function types with "C" and "C++" language linkage. This affects only type mangling, since extern "C" function objects have unmangled names.
--
--  <function-type> ::= [<CV-qualifiers>] [<exception-spec>] [Dx] F [Y] <bare-function-type> [<ref-qualifier>] E
--  <bare-function-type> ::= <signature type>+
--	# types are possible return type, then parameter types
--  <exception-spec>
--       ::= Do                # non-throwing exception-specification (e.g., noexcept, throw())
--       ::= DO <expression> E # computed (instantiation-dependent) noexcept
--       ::= Dw <type>+ E      # dynamic exception specification with instantiation-dependent types
dpFunctionType :: DP String
dpFunctionType = do
  fail "dpFunctionType: not understood yet"
  quals <- dpCvQualifiers
  excep <- dpExceptionSpec
  ts <- P.option "" $ do
    P.string "Dx" >> return " [transaction safe] "
  P.char 'F' -- marker
  extern_c <- P.option "" $ do
    P.string "Y" >> return "extern \"C\""
  bft <- dpBareFunctionType
  ref_qual <- P.option "" dpRefQualifier
  P.char 'E' -- marker
  return $ extern_c ++ ts ++ bft ++ ref_qual

dpExceptionSpec :: DP String
dpExceptionSpec = dpParseFromTable "exception-spec"
  [
    forS "Do" $ return "" -- noexcept / throw()
  , forS "DO" $ dpExpression <* P.char 'E' -- computed instantiation dependent
  , forS "Dw" $ (intercalate ", " <$> P.many1 dpType) <* P.char 'E' -- dynamic exception spec
  ]

--------------------------------------------------------------------------------
-- NAMES
--------------------------------------------------------------------------------
dpExpression :: DP String
dpExpression = dpNonTbl <|> dpTbl
  where -- TODO: operator unary/binary/ternary
        dpTbl =
          dpParseFromTable "expr" [
            --
            forS "pp_" $ ("++"++) <$> dpExpression
          , forS "mm_" $ ("--"++) <$> dpExpression
            -- ...
          , forS "dc" $ cast "dynamic_cast"
          , forS "sc" $ cast "static_cast"
          , forS "cc" $ cast "const_cast"
          , forS "rc" $ cast "reinterpret_cast"
          --
          , forS "ti" $ funcT "typeid"
          , forS "te" $ funcE "typeid"
          --
          , forS "st" $ funcT "sizeof"
          , forS "sz" $ funcE "sizeof"
          --
          , forS "at" $ funcT "alignof"
          , forS "az" $ funcE "alignof"
          --
          , forS "nx" $ funcE "noexcept"
          ]

        cast op = do
          t <- dpType
          e <- dpExpression
          return $ op ++ "<" ++ t ++ ">(" ++ e ++ ")"
        funcT op = do
          t <- dpType
          return $ op ++ "(" ++ t ++ ")"
        funcE op = do
          e <- dpExpression
          return $ op ++ "(" ++ e ++ ")"

        dpNonTbl = dpUnresolvedName
          -- dpTemplateParam <|> dpFunctionParam


--  <expr-primary>
--    ::= L <type> <value number> E                          # integer literal
--    ::= L <type> <value float> E                           # floating literal
--    ::= L <string type> E                                  # string literal
--    ::= L <nullptr type> E                                 # nullptr literal (i.e., "LDnE")
--    ::= L <pointer type> 0 E                               # null pointer template argument
--    ::= L <type> <real-part float> _ <imag-part float> E   # complex floating point literal (C 2000)
--    ::= L _Z <encoding> E                                  # external name
dpExprPrimary :: DP String
dpExprPrimary = P.char 'L' >> cases <* P.char 'E'
  where cases =
          nullptrType <|>
          P.try ptrToZero <|>
          -- stringType <|>
          valueType <|>
          -- <|> realImgType
          externName

        nullptrType = do
          dpTryString "LDn"
          return "nullptr"

        ptrToZero = do
          ty <- dpPointerType
          return $ "(" ++ ty ++ ")0"

        -- stringType = do
        --   ...

        valueType = do
          t <- dpType
          v <- dpNumber <|> dpFloat
          return $ "(" ++ t ++ ")" ++ v

        externName = do
          dpTryString "_Z"
          enc <- dpEncoding
          return enc

dpNumber :: DP String
dpNumber = show <$> dpInt

-- "Lf bf800000 E" is -1.0f on platforms conforming to IEEE 754.
dpFloat :: DP String
dpFloat = do
  hds <- P.many1 P.hexDigit
  return $
    if length hds > 8 then
        case reads hds of
          [(x,"")] -> show (doubleFromBits x)
      else
        case reads hds of
          [(x,"")] -> show (floatFromBits x)

