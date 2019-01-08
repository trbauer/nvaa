module NVT.Word128 where

import Data.Bits
import Data.Word
import qualified Data.ByteString as S

data Word128 =
  Word128 {
    wHi64 :: !Word64
  , wLo64 :: !Word64
  } deriving (Show,Eq,Ord)

getField128 :: Int -> Int -> Word128 -> Word64
getField128 off len w128
  | off + len > 128 = error "NVT.Word128.getField128: offsets out of bounds"
  | off < 64 && off + len > off + 64 = error "NVT.Word128.getField128: field cannot cross 64b boundary"
  | otherwise = shifted_value .&. mask
  where shifted_value = w `shiftR` (off `mod` 64)
          where w = if off < 64 then wLo64 w128 else wHi64 w128
        mask = (1 `shiftL` len) - 1

getField64 :: Int -> Int -> Word64 -> Word64
getField64 off len w
  | off + len >= 64 = error "NVT.Word128.getField64: offsets out of bounds"
  | otherwise = shifted_value .&. mask
  where shifted_value = w `shiftR` off
        mask = (1 `shiftL` len) - 1



