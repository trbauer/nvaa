module NVT.Bits where

import Data.Bits
import Data.Char
import Data.Word
import Debug.Trace
import Text.Printf
import qualified Data.ByteString as BS

type Binary = BS.ByteString

bConcat :: [Binary] -> Binary
bConcat = BS.concat

data Word128 =
  Word128 {
    wHi64 :: !Word64
  , wLo64 :: !Word64
  } deriving (Eq,Ord)
instance Show Word128 where
  show w128 = "0x" ++ w wHi64 ++ "`" ++ w wLo64
    where w f = printf "%016X" (f w128)
instance Read Word128 where
  readsPrec _ ('0':x:str)
    | x `elem` "xX" =
      -- TODO: could we handle expressions with readPrec?
      --   0x(1+2)`00045451?
      case span isHexDigit str of
        (ds_hi,'`':sfx)
          | not (null ds_hi) ->
          case span isHexDigit sfx of
            (ds_lo,sfx)
              | not (null ds_lo) ->
                [(Word128 (parseHex ds_hi) (parseHex ds_lo),sfx)]
              where parseHex s = read ("0x" ++ s)
            _ -> []
        _ -> []
  readsPrec _ _ = []


getField128 :: Int -> Int -> Word128 -> Word64
getField128 off len w128
  | off + len > 128 = error "NVT.Bits.getField128: offsets out of bounds"
  | off < 64 && off + len > off + 64 = error "NVT.Word128.getField128: field cannot cross 64b boundary"
  | otherwise = shifted_value .&. mask
  -- TODO: enable splits
  where shifted_value = w `shiftR` (off `mod` 64)
          where w = if off < 64 then wLo64 w128 else wHi64 w128
        mask = (1 `shiftL` len) - 1

getField64 :: Int -> Int -> Word64 -> Word64
getField64 off len w
  | off + len > 64 = error "NVT.Bits.getField64: offsets out of bounds"
  | otherwise = shifted_value .&. mask
  where shifted_value = w `shiftR` off
        mask
          | len == 64 = 0xFFFFFFFFFFFFFFFF
          | otherwise = (1 `shiftL` len) - 1

putField128 :: Int -> Int -> Word64 -> Word128 -> Word128
putField128 off len v w
  | off + len > 128 = error "NVT.Bits.putField128: offsets out of bounds"
  | off < 64 && off + len > off + 64 = error "NVT.Word128.putField128: field cannot cross 64b boundary"
  | off < 64 = w{wLo64 = putField64 (off`mod`64) len v (wLo64 w)}
  | otherwise = w{wHi64 = putField64 (off`mod`64) len v (wHi64 w)}
  -- TODO enable splits
putField64 :: Int -> Int -> Word64 -> Word64 -> Word64
putField64 off len v w
  | off + len > 64 = error "NVT.Bits.putField64: offsets out of bounds"
  | v > mask = error "NVT.Bits.putField64: value too large for field"
  | otherwise = (w .&. complement (mask`shiftL`off)) .|. shifted_value
  where shifted_value = v `shiftL` off
        mask = (1 `shiftL` len) - 1

toByteStringW128 :: Word128 -> BS.ByteString
toByteStringW128 w128 =
  toByteStringU64 (wLo64 w128) `BS.append`
    toByteStringU64 (wHi64 w128)
fromByteStringW128 :: BS.ByteString -> Word128
fromByteStringW128 bs
  | BS.length bs_hi8 < 8 = error "fromByteStringW128: insufficient bytes"
  | otherwise = Word128 (fromByteStringU64 bs_hi8) (fromByteStringU64 bs_lo8)
  where (bs_lo8,bs_hi8sfx) = BS.splitAt 8 bs
        bs_hi8 = BS.take 8 bs_hi8sfx

{-
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
-}

w128_zero :: Word128
w128_zero = Word128 0 0

instance Bits Word128 where
  (.&.) = w128binOp (.&.)
  (.|.) = w128binOp (.|.)
  xor = w128binOp xor
  complement (Word128 hi lo) = Word128 (complement hi) (complement lo)
  zeroBits = w128_zero

  -- DOESN'T WORK for shift sizes like 128
  shiftL (Word128 hi lo) off
    | off >= 64  = Word128 (lo`shiftL`off-64) 0
    | otherwise = Word128 new_hi new_lo
    where new_hi = hi `shiftL` off .|. lo `shiftR` (64 - off)
          new_lo = lo `shiftL` off
  shiftR (Word128 hi lo) off
    | off >= 64  = Word128 0 (hi`shiftR`off-64)
    | otherwise = Word128 new_hi new_lo
    where new_hi = hi `shiftR` off .|. lo `shiftR` (64 - off)
          new_lo = hi `shiftL` (64 - off) .|. lo `shiftR` off
  -- positive is left
  -- negative is right
  rotate w@(Word128 hi lo) off0
    -- rotate right
    | off < 0 = rotate w (128-off)
    -- normal bit align
    --   hhhhhhhhhhhhhhhh llllllllllllllll (each char is 4 bits)
    --     rol 4
    --   hhhhhhhhhhhhhhhl lllllllllllllllh
    | off < 64 = Word128 new_hi new_lo
    --     rol 124 = ror -4
    --   lhhhhhhhhhhhhhhh hlllllllllllllll
    | otherwise = rotate w (off-128)
    where off = off0 `mod` 128
          new_hi = hi `shiftL` off .|. lo `shiftR` (64 - off)
          new_lo = lo `shiftL` off .|. hi `shiftR` (64 - off)
  bitSize _ = 128
  bitSizeMaybe _ = Just 128
  isSigned _ = False
  testBit (Word128 hi lo) off
    | off < 64 = testBit lo off
    | otherwise = testBit hi (off - 64)
  bit off
    | off < 64 = Word128 0 (1 `shiftL` off)
    | otherwise = Word128 (1 `shiftL` off-64) 0
  popCount (Word128 hi lo) = popCount hi + popCount lo

instance FiniteBits Word128 where
  finiteBitSize _ = 128

  countLeadingZeros (Word128 hi lo)
    | clz_hi < 64 = clz_hi
    | otherwise = 64 + countLeadingZeros lo
    where clz_hi = countLeadingZeros hi
  countTrailingZeros (Word128 hi lo)
    | ctz_lo < 64 = ctz_lo
    | otherwise = 64 + countTrailingZeros hi
    where ctz_lo = countTrailingZeros lo



w128binOp :: (Word64 -> Word64 -> Word64) -> Word128 -> Word128 -> Word128
w128binOp f (Word128 hi1 lo1) (Word128 hi2 lo2) = Word128 (f hi1 hi2) (f lo1 lo2)

--------------------------
fromByteStringU8 :: BS.ByteString -> Word8
fromByteStringU8 = BS.head
fromByteStringU16 :: BS.ByteString -> Word16
fromByteStringU16 = fromByteStringG reverse
fromByteStringU16BE :: BS.ByteString -> Word16
fromByteStringU16BE = fromByteStringG id
fromByteStringU32 :: BS.ByteString -> Word32
fromByteStringU32 = fromByteStringG reverse
fromByteStringU32BE :: BS.ByteString -> Word32
fromByteStringU32BE = fromByteStringG id
fromByteStringU64 :: BS.ByteString -> Word64
fromByteStringU64 = fromByteStringG reverse
fromByteStringU64BE :: BS.ByteString -> Word64
fromByteStringU64BE = fromByteStringG id

fromByteStringG :: (FiniteBits i,Integral i) => ([Word8] -> [Word8]) -> BS.ByteString -> i
fromByteStringG reoder_bytes = go bytes zero . reoder_bytes . BS.unpack . BS.take 8
  where zero = 0
        --
        bytes = finiteBitSize zero `div` 8
        --
        -- go :: Int -> i -> [Word8] -> Word64
        go 0   i    _ = i
        go _   _    [] = error "fromByteStringG: insufficient bytes"
        go n   i    (b:bs) = go (n-1) ((i`shiftL`8) .|. fromIntegral b) bs


toByteStringU8 :: Word8 -> BS.ByteString
toByteStringU8 w8 = BS.singleton w8
--
toByteStringU16 :: Word16 -> BS.ByteString
toByteStringU16 = toByteStringG id
toByteStringU16BE :: Word16 -> BS.ByteString
toByteStringU16BE = toByteStringG reverse
--
toByteStringU32 :: Word32 -> BS.ByteString
toByteStringU32 = toByteStringG id
toByteStringU32BE :: Word32 -> BS.ByteString
toByteStringU32BE = toByteStringG reverse
--
toByteStringU64 :: Word64 -> BS.ByteString
toByteStringU64 = toByteStringG id
toByteStringU64BE :: Word64 -> BS.ByteString
toByteStringU64BE = toByteStringG reverse

toByteStringG :: (FiniteBits i,Integral i) => ([Word8] -> [Word8]) -> i -> BS.ByteString
toByteStringG reoder_bytes i = BS.pack . reoder_bytes $ packBytes bytes i
  where bytes = finiteBitSize i `div` 8

        -- packBytes :: Int -> i -> [Word8]
        packBytes 0 _ = []
        packBytes n i =
          fromIntegral (i.&.0xFF) : packBytes (n-1) (i`shiftR`8)