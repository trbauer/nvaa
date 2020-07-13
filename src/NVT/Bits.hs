module NVT.Bits where

import Data.Bits
import Data.Char
import Data.List
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

-------------------------------------------------------

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
  fromInteger 0 = w128_zero
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
w128_ones :: Word128
w128_ones = Word128 0xFFFFFFFFFFFFFFFF 0xFFFFFFFFFFFFFFFF

instance Bits Word128 where
  (.&.) = w128binOp (.&.)
  (.|.) = w128binOp (.|.)
  xor = w128binOp xor
  complement (Word128 hi lo) = Word128 (complement hi) (complement lo)
  zeroBits = w128_zero

  shiftL (Word128 hi lo) off
    | off >= finiteBitSize hi = Word128 (lo `shiftL` (off - finiteBitSize hi)) 0
    | otherwise = Word128 new_hi new_lo
    where new_hi = (hi `shiftL` off) .|. (lo `shiftR` (finiteBitSize hi - off))
          new_lo = (lo `shiftL` off)
  shiftR (Word128 hi lo) off
    | off >= finiteBitSize hi = Word128 0 (hi`shiftR`(off - finiteBitSize hi))
    | otherwise = Word128 new_hi new_lo
    where new_hi = (hi `shiftR` off) .|. (lo `shiftL`(finiteBitSize hi - off))
          new_lo = (hi `shiftL` (finiteBitSize hi - off)) .|. (lo `shiftR` off)
  -- positive is left
  -- negative is right
  rotate w@(Word128 hi lo) off0
    -- we do this with the following approach
    --   1. if 0 then nothing
    --   2. if negative (right), normalize to positive and less than word size (128);
    --       e.g. 129 mods to 1 (129 to left is 1 to the left)
    --       e.g. -1 (right 1) is 127 left
    --       e.g. -129 (right 129) == 127 left
    --     (the Haskell `mod` function does this and 'off' holds the value)
    --   3. if the bit size is smaller than our half words (64b for Word128),
    --      then we do the natural funnel shift pattern
    --   4. otherwise the shift is by N between 65 and 127, we can treat this as
    --      a shift by (N - 64) on the rotated (swapped) words
    | off == 0 = w -- 0, 128, 256, etc...
    --
    -- rotate (-129) == rotate 1 MOD
    -- rotate (129) == rotate 1
    -- rotate (1023) == rotate 127
    -- off holds normalize value
    | off < 0 = error "Word128.rotate: unreachable" -- mod should prevent this
    --
    -- step 3) SMALLER than our half word size:
    --   YYYYXXXXXXXXXXX BBBBAAAAAAAAAAA
    --  goes to:
    --   XXXXXXXXXXXBBBB AAAAAAAAAAAYYYY
    | off < finiteBitSize hi = Word128 lt64_new_hi lt64_new_lo
    --
    -- step 4) greater than 64 (but less than 127) is the same as swapping the words
    -- and rotating by (off - 64)
    --   YYYYXXXXXXXXXXX BBBBAAAAAAAAAAA
    --  goes to:
    --   AAAAAAAAAAAYYYY XXXXXXXXXXXBBBB
    -- think of this as iteratively rotating by less than 64
    | otherwise = rotate (Word128 lo hi) (off - finiteBitSize hi)
    where off = off0 `mod` finiteBitSize w
          lt64_new_hi = (hi `shiftL` off) .|. (lo `shiftR` (finiteBitSize hi - off))
          lt64_new_lo = (lo `shiftL` off) .|. (hi `shiftR` (finiteBitSize hi - off))

    --  (Word128 0xF000000000000000 0xC000000000000001)`rotate`(1) == 0xE000000000000001`8000000000000003
    --  (Word128 0xF000000000000000 0xC000000000000001)`rotate`(-1) == 0xF800000000000000`6000000000000000
  bitSize = finiteBitSize
  bitSizeMaybe = Just . finiteBitSize
  isSigned _ = False
  testBit (Word128 hi lo) off
    | off < finiteBitSize hi = testBit lo off
    | otherwise = testBit hi (off - finiteBitSize hi)
  bit off
    | off < finiteBitSize lo_bit = Word128 0 lo_bit
    | otherwise = Word128 hi_bit 0
    where lo_bit = 1 `shiftL` off
          hi_bit = 1 `shiftL` (off - finiteBitSize lo_bit)
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
w128binOp f (Word128 hi1 lo1) (Word128 hi2 lo2)
  | x == w128_zero = w128_zero
  | otherwise = x
  where x = Word128 (f hi1 hi2) (f lo1 lo2)

--------------------------
data Word256 =
  Word256 {
    wHi128 :: !Word128
  , wLo128 :: !Word128
  } deriving (Eq,Ord)

instance Show Word256 where
  show (Word256 (Word128 hh hl) (Word128 lh ll)) =
      "0x" ++ intercalate "`" (map fmt [hh,hl,lh,ll])
    where fmt x = printf "%016X" x
instance Read Word256 where
  readsPrec _ ('0':x:str)
    | x `elem` "xX" =
      -- TODO: could we handle expressions with readPrec?
      --   0x(1+2)`00045451?
      case span isHexDigit str of
        (ds_hh,'`':sfx)
          | not (null ds_hh) ->
          case span isHexDigit sfx of
            (ds_hl,sfx)
              | not (null ds_hl) ->
                case span isHexDigit sfx of
                  (ds_lh,sfx)
                    | not (null ds_lh) ->
                      case span isHexDigit sfx of
                        (ds_ll,sfx)
                          | not (null ds_ll) ->
                            [(Word256 w128h w128l,sfx)]
                            where parseHex s = read ("0x" ++ s)
                                  w128h = Word128 (parseHex ds_hh) (parseHex ds_hl)
                                  w128l = Word128 (parseHex ds_lh) (parseHex ds_ll)
                        _ -> []
                  _ -> []
            _ -> []
        _ -> []
  readsPrec _ _ = []
instance Num Word256 where
  (+) = w256BinOp (+)
  (-) = w256BinOp (-)
  (*) = w256BinOp (*)
  --
  negate = w256UnOp negate
  abs = w256UnOp abs
  signum = w256UnOp signum
  --
  fromInteger 0 = w256_zero
  fromInteger i = Word256 (Word128 hh hl) (Word128 lh ll)
    where hh = fromInteger (0xFFFFFFFFFFFFFFFF .&. (i`shiftR`192)) :: Word64
          hl = fromInteger (0xFFFFFFFFFFFFFFFF .&. (i`shiftR`128)) :: Word64
          lh = fromInteger (0xFFFFFFFFFFFFFFFF .&. (i`shiftR`64)) :: Word64
          ll = fromInteger (0xFFFFFFFFFFFFFFFF .&. i) :: Word64

w256UnOp :: (Integer -> Integer) -> Word256 -> Word256
w256UnOp f w128 =
  fromInteger (f (w256ToInteger w128))
w256BinOp :: (Integer -> Integer -> Integer) -> Word256 -> Word256 -> Word256
w256BinOp f w256a w256b =
  fromInteger (f (w256ToInteger w256a) (w256ToInteger w256b))

w256ToInteger :: Word256 -> Integer
w256ToInteger (Word256 (Word128 hh hl) (Word128 lh ll)) =
    hh_i .|. hl_i .|. lh_i .|. ll_i
  where hh_i = fromIntegral hh `shiftL` 192 :: Integer
        hl_i = fromIntegral hl `shiftL` 128 :: Integer
        lh_i = fromIntegral lh `shiftL` 64 :: Integer
        ll_i = fromIntegral ll :: Integer

w256_zero :: Word256
w256_zero = Word256 w128_zero w128_zero
w256_ones :: Word256
w256_ones = Word256 w128_ones w128_ones

instance Bits Word256 where
  (.&.) = w256binOp (.&.)
  (.|.) = w256binOp (.|.)
  xor = w256binOp xor
  complement (Word256 hi lo) = Word256 (complement hi) (complement lo)
  zeroBits = w256_zero

  -- DOESN'T WORK for shift sizes like 128
  shiftL (Word256 hi lo) off
    | off >= finiteBitSize hi = Word256 (lo`shiftL`(off - finiteBitSize hi)) 0
    | otherwise = Word256 new_hi new_lo
    where new_hi = hi `shiftL` off .|. lo `shiftR` (finiteBitSize hi - off)
          new_lo = lo `shiftL` off
  shiftR (Word256 hi lo) off
    | off >= finiteBitSize hi  = Word256 0 (hi`shiftR`(off - finiteBitSize hi))
    | otherwise = Word256 new_hi new_lo
    where new_hi = (hi `shiftR` off) .|. (lo `shiftR` (finiteBitSize hi - off))
          new_lo = (hi `shiftL` (finiteBitSize hi - off)) .|. (lo `shiftR` off)
  -- positive is left
  -- negative is right
  rotate w@(Word256 hi lo) off0
    | off == 0 = w
    | off < 0 = error "Word256.rotate: unreachable" -- mod should prevent this
    | off < 0 = rotate w (finiteBitSize hi - off)
    | off < finiteBitSize hi = Word256 lt128_new_hi lt128_new_lo
    | otherwise = rotate (Word256 lo hi) (off - finiteBitSize hi)
    where off = off0 `mod` finiteBitSize hi

          lt128_new_hi = (hi `shiftL` off) .|. (lo `shiftR` (finiteBitSize hi - off))
          lt128_new_lo = (lo `shiftL` off) .|. (hi `shiftR` (finiteBitSize hi - off))

  bitSize _ = 256
  bitSizeMaybe _ = Just 256
  isSigned _ = False
  testBit (Word256 hi lo) off
    | off < finiteBitSize hi = testBit lo off
    | otherwise = testBit hi (off - finiteBitSize hi)
  bit off
    | off < finiteBitSize lo128 = Word256 0 lo128
    | otherwise = Word256 hi128 0
    where lo128 = 1 `shiftL` off
          hi128 = 1 `shiftL` (off - finiteBitSize lo128)
  popCount (Word256 hi lo) = popCount hi + popCount lo
instance FiniteBits Word256 where
  finiteBitSize _ = 256
  countLeadingZeros (Word256 hi lo)
    | clz_hi < finiteBitSize hi = clz_hi
    | otherwise = finiteBitSize hi + countLeadingZeros lo
    where clz_hi = countLeadingZeros hi
  countTrailingZeros (Word256 hi lo)
    | ctz_lo < finiteBitSize hi = ctz_lo
    | otherwise = finiteBitSize hi + countTrailingZeros hi
    where ctz_lo = countTrailingZeros lo

w256binOp ::
  (Word128 -> Word128 -> Word128) ->
  Word256 ->
  Word256 ->
  Word256
w256binOp f (Word256 hi1 lo1) (Word256 hi2 lo2)
  | x == w256_zero = w256_zero
  | otherwise = x
  where x = Word256 (f hi1 hi2) (f lo1 lo2)

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

toWords :: FiniteBits a => (BS.ByteString -> a) -> BS.ByteString -> [a]
toWords func bs0
  | BS.length bs0 `mod` bytes_per_elem /= 0 = error $ "Bits.toWords: binary size doesn't divide bit size"
  | otherwise = go bs0
  where bytes_per_elem = finiteBitSize (func undefined)`div`8

        go bs
          | BS.null bs = []
          | otherwise =
            case BS.splitAt bytes_per_elem bs of
              (pfx,sfx) -> func pfx:go sfx



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

-- fmtBinary :: FiniteBits b => b -> String
fmtBinary b = fmtBinaryW (finiteBitSize b) b

-- fmtBinaryW :: (Show b,Bits b) => Int -> b -> String
fmtBinaryW w = toS 0 ""
  where zero = bit 0`shiftR`1
        toS n str b
          | b == zero = if null padded_str then "0" else padded_str
          | otherwise = toS (n + 1) (c:str) (b`shiftR`1)
          where padded_str = replicate (w - n) '0' ++ str
                c
                  | testBit b 0 = '1'
                  | otherwise = '0'

