module NVT.Word128 where

import Data.Bits
import Data.Word
import qualified Data.ByteString as BS

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

toByteStringW128 :: Word128 -> BS.ByteString
toByteStringW128 w128 =
  toByteStringW64 (wLo64 w128) `BS.append`
    toByteStringW64 (wHi64 w128)

toByteStringW64 :: Word64 -> BS.ByteString
toByteStringW64 = BS.pack . packBytes 8
  where packBytes :: Int ->  Word64 -> [Word8]
        packBytes 0 _ = []
        packBytes n w64 =
          fromIntegral (w64.&.0xFF) : packBytes (n-1) (w64`shiftR`8)

fromByteStringW64 :: BS.ByteString -> Word64
fromByteStringW64 = go 8 0 . BS.unpack
  where go :: Int -> Word64 -> [Word8] -> Word64
        go 0 w64 _ = w64
        go _ _ [] = error "fromByteStringW64: insufficient bytes"
        go n w64 (b:bs) = go (n+1) (w64 .|. (fromIntegral b`shiftL`((n-1)*8))) bs

fromByteStringW128 :: BS.ByteString -> Word128
fromByteStringW128 bs
  | BS.length bs_hi8 < 8 = error "fromByteStringW128: insufficient bytes"
  | otherwise = Word128 (fromByteStringW64 bs_hi8) (fromByteStringW64 bs_lo8)
  where (bs_lo8,bs_hi8sfx) = BS.splitAt 8 bs
        bs_hi8 = BS.take 8 bs_hi8sfx