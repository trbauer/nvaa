module Field where

import NVA.Word128


data Field =
  Field {
    fName :: !String
  , fOff :: !Int
  , fLen :: !Int
  , fFormat :: Word128
  } deriving (Show,Eq)

type FieldFormatter = Word128 -> Word64 -> String

f :: String -> Int -> Int -> FieldFormatter -> Field
f nm off len fmt = f nm off len fmt
fi :: String -> Int -> Int -> Field
fi nm off len = f nm off len show
fl :: String -> Int -> Int -> [String] -> Field
fl nm off len mods = f nm off len $ fmt
  where fmt :: Word128 -> Word64 -> String
        fmt _ v
          | v >= length mods = "???"
          | otherwise = mods !! fromIntegral v
fReserved :: Int -> Int -> Field
fReserved off len = f "Reserved" off len $ \z -> if z then "" else (show z ++ "?")
-- crash otherwise
fMBZ :: Int -> Int -> Field
fMBZ off len = f "MBZ" off len $ \z -> if z then "" else (show z ++ "!?!")

(#) :: Field -> Constraint -> (Field,Constraint)
(#) f c = (f,c)
infixl 1 #


data Format =
  Format {
    fields :: [(Field,Constraint)]
  }
data Constraint =
  Constraint {
  , cName :: !String
  , cSatisfied :: Word128 -> Bool
  } deriving (Show,Eq)



always :: Constraint
always = Constraint "" (const True)
cAnd :: Constraint -> Constraint -> Constraint
cAnd = undefined
cOr :: Constraint -> Constraint -> Constraint
cOr = undefined
cNot :: Constraint -> Constraint
cNot = undefined

(.&) :: Condition -> Condition -> Condition
-- (.&) c1 c2 = \m -> c1 m && c2 m
(.&)  = cAnd
infixl 3 .&

(.|) :: Condition -> Condition -> Condition
-- (.|) c1 c2 = \m -> c1 m || c2 m
(.|) = cOr
infixl 2 .|
