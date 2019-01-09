module BE where

import Control.Applicative
import Data.Bits
import Data.Word
import Debug.Trace
import qualified Text.Parsec as P


data BE =
    BECon0
  | BECon1
  | BEVarA
  | BEVarB
  | BEVarC
  | BENeg !BE
  | BEBin !BEOp !BE !BE
  deriving (Show,Eq,Ord)
data BEOp =
    BEOr
  | BEXor
  | BEAnd
  deriving (Show,Eq,Ord,Enum)

beIsVar :: BE -> Bool
beIsVar = (`elem`[BEVarA,BEVarB,BEVarC])
beIsConst :: BE -> Bool
beIsConst = (`elem`[BECon0,BECon1])

beEval :: (Word8,Word8,Word8) -> BE -> Word8
beEval (a,b,c) = eval
  where eval be =
          case be of
            BEVarA -> a
            BEVarB -> b
            BEVarC -> c
            BECon0 -> 0
            BECon1 -> complement 0
            BENeg be -> complement (eval be)
            BEBin be_op be_l be_r -> eval be_l `func` eval be_r
              where func = case be_op of {BEOr -> (.|.); BEXor -> (xor); BEAnd -> (.&.)}



beFormat :: BE -> String
beFormat be =
  case be of
    BEVarA -> "s0" -- "a"
    BEVarB -> "s1" -- "b"
    BEVarC -> "s2" -- "c"
    BECon0 -> "0"
    BECon1 -> "1"
    BENeg be
      | beIsVar be || beIsConst be -> "~" ++ beFormat be
      | otherwise -> "~(" ++ beFormat be ++ ")"
    BEBin be_op be_l be_r -> fmtT (<) be_l ++ op_str ++ fmtT (<=) be_r
      where op_str = case be_op of {BEOr -> "|"; BEXor -> "^"; BEAnd -> "&"}
            fmtT cmp be =
              case be of
                BEBin c_op _ _ | c_op `cmp` be_op -> "(" ++ beFormat be ++ ")"
                _ -> beFormat be


-- An expression is in (Tim's arbitrary) normal form if:
--  1. it's a literal (a,b, or c)
--  2. it's a negated literal (~a,~b,~c)
--  3. the subterms of all expressions are normal form
--  4. the terms of binary expressions are ordered:
--       (e1 `op` e2) implies e1 <= e2 (note ordering rules below)
--  5. if it's a negation, the subnode, is not a negation ~(~e) should be e
--  6. a binary expression is never negated
--       ~(a | b) => ~a & ~b
--       ~(a ^ b) =>  a ^ ~b
--       ~(a & b) => ~a | ~b
--  7. XOr normalizes any negation to the left term
--        a ^ ~b  => ~a ^ b
--     XOr negation elision
--        ~a ^ ~b  => a ^ b
--
--  8. constants reduction: (by 4, we only need check this ordering)
--        0 | e  = e
--        1 | e  = 1
--        0 ^ e  = e
--        1 ^ e = ~e
--        0 & e  = 0
--        1 & e  = e
--  9. tautological reduction:
--        e |  e  = e
--        e | ~e  = 1
--        e ^  e  = 0
--        e ^ ~e  = 1
--        e &  e  = e
--        e & ~e  = 1
--        ... with complements on the left
-- 10. reassociative/commutative tautological reductions
--        (_ `op` e) `op`  e  -> _ `op` (e `op`  e)
--        (_ `op` e) `op` ~e  -> _ `op` (e `op` ~e)
--        (_ `op`~e) `op`  e  -> _ `op` (e `op`  e)
--        (_ `op`~e) `op` ~e  -> _ `op` (e `op` ~e)
--        (e `op` _) `op`  e  -> (e `op`  e) `op` _
--        (e `op` _) `op` ~e  -> (e `op` ~e) `op` _
--      (and we reduce the result)
--      (we only reassociate if a reduction is possible)
--
-- An (total) ordering of expressions is given by the function `beCompare`.
--   0 < 1 < ~a < a < ~b < b < ~c < c < (~(e)) < (e1 `op` e2)
--
-- This normal form covers all outputs for three inputs (by empirical testing).
-- The largest functions have four or five terms, but I can't see how to
-- reduce them further.
--
beNormalForm :: BE -> BE
beNormalForm = nf -- trace (beFormat be) $ nf be
  where nf :: BE -> BE
        nf be = -- trace (beFormat be) $
          case be of
            -- atoms: base cases
            BECon0 -> be
            BECon1 -> be
            BEVarA -> be
            BEVarB -> be
            BEVarC -> be
            -- binary expressions
            BEBin op x y -> reduceBinExp (BEBin op (nf x) (nf y))
            -- nontrivial negation
            BENeg e -> reduceNegExp (BENeg (nf e))
          where reduceBinExp :: BE -> BE
                reduceBinExp be = -- trace ("=B=> " ++ beFormat be) $
                  case be of
                    -- constant reductions
                    BEBin BEOr  BECon0 x -> x
                    BEBin BEOr  BECon1 x -> BECon1
                    BEBin BEXor BECon0 x -> x
                    BEBin BEXor BECon1 x -> BENeg (nf x)
                    BEBin BEAnd BECon0 x -> BECon0
                    BEBin BEAnd BECon1 x -> x
                    -- tautological reductions
                    --   x `op` x
                    BEBin op x y
                      | x == y ->
                        case op of
                          BEOr -> x
                          BEAnd -> x
                          BEXor -> BECon0
                    --   x `op` ~x
                    BEBin op x (BENeg y)
                      | x == y ->
                      case op of
                        BEOr  -> BECon1
                        BEAnd -> BECon0
                        BEXor -> BECon1
                    --   ~x `op` x
                    BEBin op (BENeg x) y
                      | x == y ->
                      case op of
                        BEOr  -> BECon1
                        BEAnd -> BECon0
                        BEXor -> BECon1
                    -- reassociative tautological reduction repeated variable means
                    --   x = y or x = ~y or ~x = y
                    BEBin op (BEBin op2 x y) z
                      --   (x `op` y) `op` y
                      | op == op2 && canReduce x z ->
                        nf (BEBin op (BEBin op x z) y)
                      --   (x `op` y) `op` x
                      | op == op2 && canReduce y z ->
                        nf (BEBin op x (BEBin op y z))
                    BEBin op x (BEBin op2 y z)
                      --   x `op` (x `op` z)
                      --   x `op` (y `op` x)
                      | op == op2 && canReduce x y ->
                        nf (BEBin op (BEBin op x y) z)
                      | op == op2 && canReduce x z ->
                        nf (BEBin op (BEBin op x z) y)
                    -- distributive tautological reduction
                    BEBin BEAnd x (BEBin op y z)
                      --   x & (X `op` z) => x&X `op` x&z
                      --   x & (y `op` X) => x&y `op` x&X
                      | canReduce x y || canReduce x z ->
                        nf (BEBin op (BEBin BEAnd x y) (BEBin BEAnd x z))
                    BEBin BEAnd (BEBin op x y) z
                      --   (x `op` y) & X => x&x `op` x&y
                      --   (y `op` x) & X => x&x `op` x&z
                      | canReduce x z || canReduce y z ->
                        nf (BEBin op (BEBin BEAnd x z) (BEBin BEAnd y z))

                    -- Xor negation elision
                    --   ~a ^ ~b => a ^ b
                    BEBin BEXor (BENeg x) (BENeg y) ->
                      nf (BEBin BEXor x y)
                    -- Xor negation rotation (normalization
                    --   ~x ^ y => x ^ ~y
                    BEBin BEXor (BENeg x) y ->
                      nf (BEBin BEXor x (BENeg y))

                    -- term ordering
                    BEBin op x y
                      -- e2 `op` e1 -> e1 `op` e2 given e1 < e2
                      | beCompare x y == GT -> -- trace ("term ordering: " ++ beFormat be ++ " => " ++ beFormat r) $
                        nf (BEBin op y x)
                      -- where r = BEBin op y x
                    -- commuatative ordering
                    -- (~a&~c)&~b => (~a&~b)&~c
                    BEBin op (BEBin op2 x y) z
                      | op == op2 && beCompare z y == LT ->
                        -- trace ("commute order: " ++ beFormat be)
                        nf (BEBin op (BEBin op x z) y)
                    -- ~c&(~a&~b) => (~a&~b)&~c
                    -- favor left associativity
                    BEBin op x (BEBin op2 y z)
                      | op == op2 -> -- trace ("reassociate: " ++ beFormat be ++ " => " ++ beFormat r) $
                        -- TERM ORDER: ~a|a^b|a&b    => a&b|(~a|a^b)
                        -- REASSOC:     a&b|(~a|a^b) => a&b|~a|a^b
                        --
                        -- NB. we only attempt to reduce the the new expression x `op` y
                        -- This avoids a cycle. E.g.
                        --   ~a|a^b|a&b => a&b|(~a|a^b) [TERM ORDER]
                        --   a&b|(~a|a^b) => a&b|~a|a^b [REASSOC]
                        --   a&b|~a|a^b => ~a|a^b|a&b   [TERM ORDER]
                        (BEBin op (nf (BEBin op x y)) z)
                    _ -> be -- arguments are already normalized on the way in
                    -- BEBin op x y -> BEBin op (nf x) (nf y)

                canReduce :: BE -> BE -> Bool
                canReduce a b = a == b || a == BENeg b || BENeg a == b

                reduceNegExp :: BE -> BE
                reduceNegExp be =
                  case be of
                    BENeg BEVarA -> be
                    BENeg BEVarB -> be
                    BENeg BEVarC -> be
                    -- _ | trace ("=U=> " ++ beFormat be) False -> undefined

                    BENeg BECon1 -> BECon0
                    BENeg BECon0 -> BECon1
                    BENeg (BENeg be) -> nf be
                    -- DeMorgan's
                    -- ~(x & y) = ~x | ~y
                    BENeg (BEBin BEAnd x y) -> BEBin BEOr (nf (BENeg x)) (nf (BENeg y))
                    -- ~(x ^ y) =  x ^ ~y
                    BENeg (BEBin BEXor x y) -> BEBin BEXor (nf x) (nf (BENeg y))
                    -- ~(x | y) = ~x & ~y
                    BENeg (BEBin BEOr x y) -> BEBin BEAnd (nf (BENeg x)) (nf (BENeg y))



compareChain :: [Ordering] -> Ordering
compareChain ords =
  case dropWhile (==EQ) ords of
    [] -> EQ
    (ord:_) -> ord

beCompare :: BE -> BE -> Ordering
beCompare e1 e2 =
  case (e1,e2) of
    (BEBin op1 el1 er1,BEBin op2 el2 er2) ->
      compareChain [beCompare el1 el2, beCompare el1 el2, compare op1 op2]
    (BEBin op1 el1 _,BENeg er) ->
      compareChain [beCompare el1 er, GT] -- (a`op`b) > ~a
    (BEBin op1 el1 _,er) ->
      compareChain [beCompare el1 er, GT] -- (a`op`b) > a
    (BENeg e1,BEBin op el _) ->
      compareChain [beCompare e1 el, LT] -- ~a < (a`op`b)
    (e1,BEBin op el _) ->
      compareChain [beCompare e1 el, LT] --  a < (a`op`b)
    (BENeg e1, BENeg e2) -> beCompare e1 e2
    (BENeg e1, e2)
      | e1 == e2 -> GT                 -- ~e > e
      | otherwise -> beCompare e1 e2
    (e1, BENeg e2)
      | e1 == e2 -> LT                 -- e < ~e
      | otherwise -> beCompare e1 e2
    (BECon0,BECon0) -> EQ
    (BECon0,_)      -> LT
    (BECon1,BECon1) -> EQ
    (BECon1,BECon0) -> GT
    (BECon1,_)      -> LT
    (v,e)
      | beIsVar v && beIsConst e -> GT
      | beIsVar v && beIsVar e   ->
        case (v,e) of
          (BEVarA,BEVarA) -> EQ
          (BEVarA,_) -> LT
          (BEVarB,BEVarA) -> GT
          (BEVarB,BEVarB) -> EQ
          (BEVarB,_) -> LT
          (BEVarC,BEVarA) -> GT
          (BEVarC,BEVarB) -> GT
          (BEVarC,BEVarC) -> EQ
          (BEVarC,_) -> LT

------------------------------------------------------------------
-- parsing code

beParse :: String -> BE
beParse inp =
  case beParseG inp of
    Left err -> error $ err
    Right be -> be

beParseG :: String -> Either String BE
beParseG inp =
  case parseP ((P.spaces >> pBE) <* P.eof) inp of
    Left err -> Left $ show err
    Right be -> Right be

parseP :: P a -> String -> Either P.ParseError a
parseP p inp = P.runParser p () "<interactive>" inp

type P = P.Parsec String ()

pLexeme :: P a -> P a
pLexeme = (<* P.spaces)

-- a&b&c
pBE :: P BE
pBE = pOrBE

pOrBE :: P BE
pOrBE = pBinExpr ('|',BEOr) pXorBE

pXorBE :: P BE
pXorBE = pBinExpr ('^',BEXor) pAndBE

pAndBE :: P BE
pAndBE = pBinExpr ('&',BEAnd) pNegBE

pBinExpr :: (Char,BEOp) -> P BE -> P BE
pBinExpr (c,op) pLowerExpr =
-- p op x (parse one or more p)
  P.chainl1 pLowerExpr ((pLexeme (P.char c)) >> return (BEBin op))


pNegBE :: P BE
pNegBE = pNeg <|> pAtom
  where pNeg = do
          pLexeme (P.char '~')
          be <- pNegBE
          return $ BENeg be


-- a, b, c, 0, 1
pAtom :: P BE
pAtom = pLits <|> pParens
  where pLit c be = pLexeme (P.char c >> return be)
        -- pLit2 s =
        pLits = pLexeme $
                    pLit 'a' BEVarA
                <|> pLit 'b' BEVarB
                <|> pLit 'c' BEVarC
                <|> pLit '0' BECon0
                <|> pLit '1' BECon1

        pParens = do
          pLexeme (P.char '(')
          be <- pBE
          pLexeme (P.char ')')
          return be
