{-# LANGUAGE PatternSynonyms #-}
module NVT.EnumSet where


import Data.Bits
import Data.Word
import Data.List(foldl')

type EnumSet = EnumBitSet Int
type EnumSet64 = EnumBitSet Word64

-- this fails since we since fromList cannot infer the integral type
newtype EnumBitSet i a = EnumBitSet i
  deriving (Eq,Ord,Read)

pattern EnumSetEMPTY :: (Eq i,Num i) => EnumBitSet i a
pattern EnumSetEMPTY = EnumBitSet 0

instance (Enum a,Show a,Bits i) => Show (EnumBitSet i a) where
  show es = "fromList " ++ show (toList es)

fromList :: (Enum a,Bits i,Num i) => [a] -> EnumBitSet i a
fromList = fromListG

fromListG :: (Enum a,Bits i,Num i) => [a] -> EnumBitSet i a
fromListG = EnumBitSet . foldl' addE 0
  where addE bits a =
            case bitSizeMaybe bits of
              Nothing -> setBit bits ix
              Just n
                | ix > n -> error "EnumSet.fromList" "bit index for enum elem too large for bitset"
                | otherwise -> setBit bits ix
          where ix = fromEnum a

singleton :: (Enum a, Bits i, Num i) => a -> EnumBitSet i a
singleton = flip insert empty

toList :: (Enum a, Bits i) => EnumBitSet i a -> [a]
toList es = filter (`member`es) [toEnum 0 ..]

empty :: (Enum a,Bits i,Num i) => EnumBitSet i a
empty = EnumBitSet 0

elem :: (Enum a, Bits i) => a -> EnumBitSet i a -> Bool
elem = member

null :: (Enum a, Bits i,Num i) => EnumBitSet i a -> Bool
null (EnumBitSet bits) = bits == 0

member :: (Enum a, Bits i) => a -> EnumBitSet i a -> Bool
member a (EnumBitSet bits) = testBit bits (fromEnum a)

insert :: (Enum a, Bits i) => a -> EnumBitSet i a -> EnumBitSet i a
insert a (EnumBitSet bits) = EnumBitSet (setBit bits (fromEnum a))

remove :: (Enum a, Bits i) => a -> EnumBitSet i a -> EnumBitSet i a
remove a (EnumBitSet bits) = EnumBitSet (clearBit bits (fromEnum a))

subtract :: (Enum a, Bits i) => EnumBitSet i a -> EnumBitSet i a -> EnumBitSet i a
subtract (EnumBitSet bs1) (EnumBitSet bs2) = EnumBitSet (bs1.&.complement bs2)

removeAll :: (Enum a, Bits i) => [a] -> EnumBitSet i a -> EnumBitSet i a
removeAll as (EnumBitSet bits) =  EnumBitSet $! foldl' (\bits a -> clearBit bits (fromEnum a)) bits as

union :: (Enum a, Bits i) => EnumBitSet i a -> EnumBitSet i a -> EnumBitSet i a
union (EnumBitSet bs1) (EnumBitSet bs2) = (EnumBitSet (bs1 .|. bs2))

unions :: (Enum a, Bits i, Num i) => [EnumBitSet i a] -> EnumBitSet i a
unions = foldl' union empty

intersect :: (Enum a, Bits i, Num i) => EnumBitSet i a -> EnumBitSet i a -> EnumBitSet i a
intersect (EnumBitSet bits1) (EnumBitSet bits2) = EnumBitSet (bits1 .&. bits2)
