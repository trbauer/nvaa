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

bAppend :: Binary -> Binary -> Binary
bAppend = BS.append

bEmpty :: Binary
bEmpty = BS.empty

bZeros :: Int -> Binary
bZeros k = BS.pack (replicate k 0)

bSize :: Binary -> Int
bSize = BS.length

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
instance Num Word128 where
  (+) = w128BinOp (+)
  (-) = w128BinOp (-)
  (*) = w128BinOp (*)
  --
  negate = w128UnOp negate
  abs = w128UnOp abs
  signum = w128UnOp signum
  --
  fromInteger i = Word128 hi lo
    where hi = fromInteger (0xFFFFFFFFFFFFFFFF .&. (i`shiftR`64)) :: Word64
          lo = fromInteger (0xFFFFFFFFFFFFFFFF .&. i) :: Word64

w128UnOp :: (Integer -> Integer) -> Word128 -> Word128
w128UnOp f w128 =
  fromInteger (f (w128ToInteger w128))
w128BinOp :: (Integer -> Integer -> Integer) -> Word128 -> Word128 -> Word128
w128BinOp f w128a w128b =
  fromInteger (f (w128ToInteger w128a) (w128ToInteger w128b))

w128ToInteger :: Word128 -> Integer
w128ToInteger (Word128 hi lo) = hi_i .|. lo_i
  where hi_i = fromIntegral hi `shiftL` 64 :: Integer
        lo_i = fromIntegral lo :: Integer


{-
getFieldBits :: FiniteBits b => Int -> Int -> b -> Word64
getFieldBits off len b =
  | off + len > finiteBitSize b = error "NVT.Bits.getFieldBits: offsets out of bounds for type"
  -- TODO: enable splits
  | off < 64 && off + len > off + 64 = error "NVT.Bits.getField: field cannot cross 64b boundary"
  | otherwise = shifted_value .&. mask
  where shifted_value = w `shiftR` (off `mod` 64)
          where w = if off < 64 then wLo64 w128 else wHi64 w128
        mask = (1 `shiftL` len) - 1
-}

getField128 :: Int -> Int -> Word128 -> Word64
getField128 off len w128
  | off + len > 128 = error "NVT.Bits.getField128: offsets out of bounds"
  | off < 64 && off + len > off + 64 = error "NVT.Bits.getField128: field cannot cross 64b boundary"
  | otherwise = shifted_value .&. mask
  -- TODO: enable splits
  where shifted_value = w `shiftR` (off `mod` 64)
          where w = if off < 64 then wLo64 w128 else wHi64 w128
        mask =  getMask64 len



getField64 :: Int -> Int -> Word64 -> Word64
getField64 off len w
  | off + len > 64 = error "NVT.Bits.getField64: offsets out of bounds"
  | otherwise = shifted_value .&. mask
  where shifted_value = w `shiftR` off
        mask = getMask64 len

getMask64 :: Int -> Word64
getMask64 len
  | len == 64 = 0xFFFFFFFFFFFFFFFF
  | otherwise = (1 `shiftL` len) - 1

putField128 :: Int -> Int -> Word64 -> Word128 -> Word128
putField128 off len v w
  | off + len > 128 = error "NVT.Bits.putField128: offsets out of bounds"
  | off < 64 && off + len > off + 64 = error "NVT.Bits.putField128: field cannot cross 64b boundary"
  | off < 64 = w{wLo64 = putField64 (off`mod`64) len v (wLo64 w)}
  | otherwise = w{wHi64 = putField64 (off`mod`64) len v (wHi64 w)}
  -- TODO enable splits
putField64 :: Int -> Int -> Word64 -> Word64 -> Word64
putField64 off len v w
  | off + len > 64 = error "NVT.Bits.putField64: offsets out of bounds"
  | v > mask = error "NVT.Bits.putField64: value too large for field"
  | otherwise = (w .&. complement (mask`shiftL`off)) .|. shifted_value
  where shifted_value = v `shiftL` off
        mask = getMask64 len

-- TODO: deprecate
toByteStringW128 :: Word128 -> BS.ByteString
toByteStringW128 = toByteStringU128LE
fromByteStringW128 :: BS.ByteString -> Word128
fromByteStringW128 = fromByteStringU128LE

toByteStringU128LE :: Word128 -> BS.ByteString
toByteStringU128LE w128 =
  toByteStringLE (wLo64 w128) `BS.append`
    toByteStringLE (wHi64 w128)
fromByteStringU128LE :: BS.ByteString -> Word128
fromByteStringU128LE bs
  | BS.length bs_hi8 < 8 = error "fromByteStringU128LE: insufficient bytes"
  | otherwise = Word128 (fromByteStringLE bs_hi8) (fromByteStringLE bs_lo8)
  where (bs_lo8,bs_hi8sfx) = BS.splitAt 8 bs
        bs_hi8 = BS.take 8 bs_hi8sfx

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



w128binOp ::
  (Word64 -> Word64 -> Word64) ->
  Word128 ->
  Word128 ->
  Word128
w128binOp f (Word128 hi1 lo1) (Word128 hi2 lo2) =
  Word128 (f hi1 hi2) (f lo1 lo2)

--------------------------
fromByteStringU8 :: BS.ByteString -> Word8
fromByteStringU8 = BS.head
fromByteStringU16LE :: BS.ByteString -> Word16
fromByteStringU16LE = fromByteStringLE
fromByteStringU16BE :: BS.ByteString -> Word16
fromByteStringU16BE = fromByteStringBE
fromByteStringU32LE :: BS.ByteString -> Word32
fromByteStringU32LE = fromByteStringLE
fromByteStringU32BE :: BS.ByteString -> Word32
fromByteStringU32BE = fromByteStringBE
fromByteStringU64LE :: BS.ByteString -> Word64
fromByteStringU64LE = fromByteStringLE
fromByteStringU64BE :: BS.ByteString -> Word64
fromByteStringU64BE = fromByteStringBE

fromByteStringLE :: (FiniteBits i,Integral i) => BS.ByteString -> i
fromByteStringLE = fromByteStringG reverse
fromByteStringBE :: (FiniteBits i,Integral i) => BS.ByteString -> i
fromByteStringBE = fromByteStringG id

fromByteStringG :: (FiniteBits i,Integral i) => ([Word8] -> [Word8]) -> BS.ByteString -> i
fromByteStringG reoder_bytes =
    go num_bytes zero . reoder_bytes . BS.unpack . BS.take num_bytes
  where zero = 0
        --
        num_bytes = finiteBitSize zero `div` 8
        --
        -- go :: Int -> i -> [Word8] -> Word64
        go 0   i    _ = i
        go _   _    [] = error "fromByteStringG: insufficient bytes"
        go n   i    (b:bs) = go (n-1) ((i`shiftL`8) .|. fromIntegral b) bs


toByteStringU8 :: Word8 -> BS.ByteString
toByteStringU8 w8 = BS.singleton w8
--
toByteStringU16LE :: Word16 -> BS.ByteString
toByteStringU16LE = toByteStringLE
toByteStringU16BE :: Word16 -> BS.ByteString
toByteStringU16BE = toByteStringBE
--
toByteStringU32LE :: Word32 -> BS.ByteString
toByteStringU32LE = toByteStringLE
toByteStringU32BE :: Word32 -> BS.ByteString
toByteStringU32BE = toByteStringBE
--
toByteStringU64LE :: Word64 -> BS.ByteString
toByteStringU64LE = toByteStringLE
toByteStringU64BE :: Word64 -> BS.ByteString
toByteStringU64BE = toByteStringBE

toByteStringLE :: (FiniteBits i,Integral i) => i -> BS.ByteString
toByteStringLE = toByteStringG id
toByteStringBE :: (FiniteBits i,Integral i) => i -> BS.ByteString
toByteStringBE = toByteStringG reverse
--
toByteStringG :: (FiniteBits i,Integral i) => ([Word8] -> [Word8]) -> i -> BS.ByteString
toByteStringG reoder_bytes i = BS.pack . reoder_bytes $ packBytes bytes i
  where bytes = finiteBitSize i `div` 8

        -- packBytes :: Int -> i -> [Word8]
        packBytes 0 _ = []
        packBytes n i =
          fromIntegral (i.&.0xFF) : packBytes (n-1) (i`shiftR`8)