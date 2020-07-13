module NVT.Floats where

import NVT.Bits

import Data.Bits
import Data.Int
import Data.Word
import Debug.Trace
import Unsafe.Coerce(unsafeCoerce)
import GHC.Float
import Text.Printf


doubleToBits :: Double -> Word64
doubleToBits = unsafeCoerce
doubleFromBits :: Word64 -> Double
doubleFromBits = unsafeCoerce
-- TODO: remove
bitsToDouble :: Word64 -> Double
bitsToDouble = doubleFromBits


floatToBits :: Float -> Word32
floatToBits = unsafeCoerce
floatFromBits :: Word32 -> Float
floatFromBits = unsafeCoerce
-- TODO: remove it
bitsToFloat :: Word32 -> Float
bitsToFloat = floatFromBits

-- doubleToFloat :: Double -> Float
-- doubleToFloat = double2Float -- realToFrac
-- floatToDouble :: Float -> Double
-- floatToDouble = float2Double -- realToFrac

-- class CustomFloat f where
--   cfToBits :: FiniteBits b => f -> b
--   cfFromBits :: FiniteBits b => b -> f
--
--   cfEXP_BITS :: f -> Int
--   cfMNT_BITS :: f -> Int
--
--   cfConvert :: (CustomFloat f1,CustomFloat f2) => f1 -> f2


-----------------------------------------------------------------------------

f64_sign_bit :: Word64
f64_sign_bit = 0x8000000000000000
f64_exp_mask :: Word64
f64_exp_mask = 0x7FF0000000000000
f64_qnan_bit :: Word64
f64_qnan_bit = 0x0008000000000000

f32_sign_bit :: Word32
f32_sign_bit = 0x80000000
f32_exp_mask :: Word32
f32_exp_mask = 0x7F800000
f32_qnan_bit :: Word32
f32_qnan_bit = 0x00400000
f32_mnt_mask :: Word32
f32_mnt_mask = (f32_qnan_bit`shiftL`1) - 1

f16_sign_bit :: Word16
f16_sign_bit = 0x8000
f16_exp_mask :: Word16
f16_exp_mask = 0x7C00
f16_qnan_bit :: Word16
f16_qnan_bit = 0x0200
f16_mnt_mask :: Word16
f16_mnt_mask = (f16_qnan_bit`shiftL`1) - 1

-- deals with NaN
floatToDouble :: Float -> Double
floatToDouble f32
  | f32 == f32 = float2Double f32
  | otherwise = bitsToDouble (w64_sign .|. w64_inf_exp .|. w64_qnan .|. w64_payload)
  where w32 = floatToBits f32
        w64_sign = if testBit w32 31 then f64_sign_bit else 0
        w64_qnan = if testBit w32 22 then f64_qnan_bit else 0
        w64_inf_exp = 0x7FF0000000000000
        -- following IGA rules here, use the bottom for the payload
        w64_payload = fromIntegral $ getBits 0 22 w32

-- deals with NaN
doubleToFloat :: Double -> Float
doubleToFloat f64
  | f64 == f64 = double2Float f64
  | otherwise = bitsToFloat (w32_sign .|. w32_exp_inf .|. w32_qnan .|. w32_nan_payload)
  where w64 = doubleToBits f64
        w32_sign = if testBit w64 63 then f32_sign_bit else 0
        w32_qnan = if testBit w64 51 then f32_qnan_bit else 0
        w32_exp_inf = f32_exp_mask
        w32_nan_payload = if f64_payload_lo22 /= 0 then f64_payload_lo22 else 1
            where f64_payload_lo22 = fromIntegral (getBits 0 22 w64) :: Word32

getBits off len val = ((val `shiftR` off) .&. ((1`shiftL`len) - 1))

-- deals with NaN
negateDouble :: Double -> Double
negateDouble d
  | d == d = negate d
  | otherwise = bitsToDouble w64_negated
  where w64_negated = doubleToBits d `xor` f64_sign_bit

-- http://www.open-std.org/jtc1/sc22/wg14/www/docs/n2016.pdf
newtype Half = Half Word16

halfToBits :: Half -> Word16
halfToBits (Half w16) = w16
halfFromBits :: Word16 -> Half
halfFromBits = Half

instance Show Half where
--  -- http://www.open-std.org/jtc1/sc22/wg14/www/docs/n2016.pdf
--  -- 5.1 uses "sf"
--  show = (++"sf") . show . halfToFloat
  show = show . halfToFloat
instance Read Half where
  readsPrec p = map procResult . reads
    where procResult (val,'s':'f':sfx) = (val,sfx)
          procResult x = x
instance Eq Half where
  (==) = hlift2 (==)
instance Ord Half where
  compare = hlift2 compare
instance Enum Half where
  toEnum = Half . toEnum
  fromEnum (Half w16) = fromEnum w16

-- instance Floating Float -- Defined in `GHC.Float'
-- instance Fractional Float -- Defined in `GHC.Float'
instance Num Half where
  (+) = hlift2RePack (+)
  (-) = hlift2RePack (-)
  (*) = hlift2RePack (*)
  negate h@(Half w16)
    | halfIsNaN h = Half (complementBit w16 15 .|. f16_qnan_bit)
    | otherwise = Half (complementBit w16 15)
  abs h@(Half w16)
    | halfIsNaN h = Half (clearBit w16 15 .|. f16_qnan_bit)
    | otherwise = Half (clearBit w16 15)
  signum = floatToHalf . signum . halfToFloat
  fromInteger = floatToHalf . fromInteger
instance Real Half where
  toRational = toRational . halfToFloat
{-
instance RealFloat Float where
  floatRadix _ = 2
  floatDigits _  = 10
  floatRange _ = (-16,15) :: a -> (Int, Int)
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  exponent :: a -> Int
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  isNaN :: a -> Bool
  isInfinite :: a -> Bool
  isDenormalized :: a -> Bool
  isNegativeZero :: a -> Bool
  isIEEE :: a -> Bool
  atan2 :: a -> a -> a
-}

-- instance RealFloat Float where -- Defined in `GHC.Float'
-- instance RealFrac Float where -- Defined in `GHC.Float'

-- hlift :: (Float -> a) -> Half -> a
-- hlift = halfToFloat

hlift2 :: (Float -> Float -> a) -> Half -> Half -> a
hlift2 func h1 h2 = func (halfToFloat h1) (halfToFloat h2)

hlift2RePack :: (Float -> Float -> Float) -> Half -> Half -> Half
hlift2RePack func = hlift2 (\h1 -> floatToHalf . func h1)

halfIsNaN :: Half -> Bool
halfIsNaN (Half w16) =
  (f16_exp_mask .&. w16) == f16_exp_mask &&
  (f16_mnt_mask .&. w16) /= 0



-- from IEEE-754-2008 section 4.3
-- diagrams show:
--    '|' as FP16 values and 'X' as the FP32 (IEEE "infinitely precise" value)
--    'E' means nearest even FP16 and 'O' nearest odd
--
data Round =
    -- round to nearest even (nearest and then to even if tied)
    --
    --   ...|..X....|....
    --      ^<<< no even check because it's closer to low value
    --   ...|.....X.|....
    --            >>^ no even check because it's closer to low value
    --   ...O...X...E....
    --          >>>>^ ties to even
    --   ...E...X...O....
    --      ^<<<< ties to even
    --
    -- > roundTiesToEven, the floating-point number nearest to the infinitely
    -- > precise result shall be delivered; if the two nearest floating-point
    -- > numbers bracketing an unrepresentable infinitely precise result are
    -- > equally near, the one with an even least significant digit shall be delivered
    --
    RoundE
    --
    -- round to nearest away from zero
    --
    -- > roundTiesToAway, the floating-point number nearest to the infinitely
    -- > precise result shall be delivered; if the two nearest floating-point
    -- > numbers bracketing an unrepresentable infinitely precise result are
    -- > equally near, the one with larger magnitude shall be delivered.
  | RoundA
    --
    -- round towards positive infinity
    --
    --   ...|..X....|....
    --         >>>>>^ closest >= value
    --
    -- > roundTowardPositive, the result shall be the format's floating-point number
    -- > (possibly +inf) closest to and no less than the infinitely precise result
    --
  | RoundP
    --
    -- round towards negative infinity
    --
    --   ...|..X....|....
    --      ^<<< closest <= value
    --
    -- > roundTowardNegative, the result shall be the format's floating-point number
    -- > (possibly -inf) closest to and no greater than the infinitely precise result
    --
  | RoundN
    --
    -- round towards zero
    --
    --   -inf ...|..X....|.... -0.0
    --              >>>>>^ closest to 0

    --   +0.0 ...|..X....|.... +inf
    --           ^<<<< closest to 0
    --
    -- > roundTowardZero, the result shall be the format's floating-point number
    -- > closest to and no greater in magnitude than the infinitely precise result.
  | RoundZ
  deriving (Show,Eq)

floatToHalf :: Float -> Half
floatToHalf = floatToHalfRound RoundE

floatToHalfRound :: Round -> Float -> Half
floatToHalfRound r = Half . floatBitsToHalfBits r . floatToBits

quantizeMantissaRNE :: Int -> Word32 -> Word32
quantizeMantissaRNE mnt_len w32
  | (w32 .&. f32_exp_mask) == f32_exp_mask = w32 -- inf/nan don't change
  | otherwise = quantized_result
  where m32 = w32 .&. f32_mnt_mask
        e32 = (w32 .&. f32_exp_mask) `shiftR` 23

        lsb = (mnt_len <= 24) && odd (w32`shiftR`(24 - mnt_len))
        half = (mnt_len <= 23) && odd (w32`shiftR`(23 - mnt_len))

        remainder =
          (mnt_len < 23) &&
          (w32 .&. complement (0xFFFFFFFF`shiftL`(23 - mnt_len))) /= 0

        -- truncate mantissa bits
        w32_trunc :: Word32
        w32_trunc = w32 .&.
          (0xFFFFFFFF`shiftL`(if (mnt_len < 24) then (24 - mnt_len) else 0))

        quantized_result :: Word32
        quantized_result
          -- Round by adding the differences between two numbers that
          -- have the same exponent and the appropriate LSB bit set.
          | mnt_len < 24 && half && (lsb || remainder) = floatToBits f_result
          | otherwise = w32_trunc
          where one = w32_trunc .&. 0xFF800000
                one_p_lsb = one .|. (1`shiftL`(24 - mnt_len))
                f_result =
                  floatFromBits w32_trunc +
                  (floatFromBits one_p_lsb - floatFromBits one)



floatBitsToHalfBits :: Round -> Word32 -> Word16
floatBitsToHalfBits r w32_unquant
  | w32 .&. complement f32_sign_bit == 0 = f16_sign -- +/-0.0
  | (w32 .&. f32_exp_mask) == f32_exp_mask = f16_infinity -- +/-infinity
  | (w32 .&. f32_exp_mask) == f32_exp_mask && m32 /= 0 = f16_nan -- nan
  | otherwise = non_nan -- normal or subnormal
  where m32 = w32 .&. f32_mnt_mask
        e32 = (w32 .&. f32_exp_mask) `shiftR` 23

        w32
--          | r == RoundE = quantizeMantissaRNE 11 w32_unquant
          | otherwise = w32_unquant

        f32_mnt_bits = 23
        f16_mnt_bits = 10
        mnt_diff = f32_mnt_bits - f16_mnt_bits
        bias_difference = 127 - 15
        e32_min_representible = (127 - 15) - 10 -- 102 (0x66)
        f16_max_exp = 0x1E

        non_nan
          --
          -- e16 overflows fp16 exponent after bias fixup ==> +/-INFINITY
          | e32 > bias_difference + f16_max_exp = f16_infinity
          --
          -- too small: rounds to +/-0
          | e32 < e32_min_representible = f16_sign
          --
--          | r == RoundE = round_even_non_nan

          -- denorm value
          --   0x66 is (127 - 15) - 10 (10b of mantissa we can use in fp16)
          | e32 <= bias_difference && e32 >= e32_min_representible =
              fixDenormLoop e32 (m32 .|. (f32_qnan_bit`shiftL`1))
          --
          -- normalized value
          | otherwise =
            completeNonNan
              ((e32 - bias_difference)`shiftL`f16_mnt_bits)
              m32
          where fixDenormLoop e32 m32
                  | e32 <= bias_difference = fixDenormLoop (e32+1) (m32`shiftR`1)
                  -- | otherwise = completeNonNan 0 m32
                  | otherwise = f16_sign .|. fromIntegral (m32`shiftR`(23 - 10))

                round_even_non_nan :: Word16
                round_even_non_nan
                  | round_off == 0x4000 = finish e16 (m16+1)
                  | (round_off .&. 0x1000) /= 0 = handle_tie
                  | otherwise = finish e16 m16
                  where e16 = e32 - 0x70
                        m16 = m32`shiftR`(23-10)
                        round_off = m32 .&. 0x7FFF -- bottom 11 bits
                        handle_tie
                          | m16_impl .&. 0x800 /= 0 = finish (e16+1) (m16_impl`shiftR`1)
                          | otherwise = finish (e16+1) (m16_impl`shiftR`1)
                          where m16_impl = ((m16 .|. 0x400) + 1) -- replace implied one and round up

                        finish :: Word32 -> Word32 -> Word16
                        finish e16 m16 = trace (printf "%X %X" e16 m16)$
                          f16_sign_bit .|. fromIntegral ((e16`shiftL`23) .|. (m16 .&. 0x3FF))

                completeNonNan :: Word32 -> Word32 -> Word16
                completeNonNan e16 m32 =
                    case r of
                      RoundZ -> f16_sign .|. fromIntegral (e16 .|. m16)
                      _ ->
                          -- trace (printf "%08X" w32 ++ "==>" ++ fmtBinaryW 10 m16 ++ "`" ++ fmtBinaryW 13 roundoff ++  " = " ++ printf "%X" roundoff) $
                            rounded

                  where m16 = m32`shiftR`mnt_diff

                        -- fp32: S`EEEEEEEE`MMMMMMMMMMMMMMMMMMMMMMM
                        -- fp16:    S`EEEEE`MMMMMMMMMM|||||||||||||
                        --                            ^ roundoff  ^
                        --                           add 1 if >= 0x1000

                        -- bits rounded off
                        roundoff = (m32 .&. ((1`shiftL`mnt_diff) - 1))
                        -- bit 12... (23 - 10) is the high roundoff
                        rounded_carry_in :: Word16
                        rounded_carry_in
                          | roundoff == 0 = 0
                          | otherwise =
                            case r of
                              RoundE
                                -- closer to 0 (e.g. decimal YY.XX4...)
                                | roundoff < 0x1000 -> 0
                                -- closer to 1 (e.g. decimal YY.XX6...)
                                | roundoff > 0x1000 -> 1
                                -- ties to even (e.g. decimal YY.XX5...)
                                | otherwise -> if odd m16 then 1 else 0
                              RoundP -> if f_negative then 0 else 1
                              RoundN -> if f_negative then 1 else 0

                          -- the rounding carry in bit is 1 or 0 and
                          --   added to the new mantissa (potentially incrementing it)
                          --   if mnt overflows it will overflow into exp (mnt will be 0's in this case)
                          --   if exp saturates to all 1's that will be a clean infinity pattern
                          --     (exp cannot overflow to 0 since caller would have caught that
                          --     as infinity earlier)
                        rounded :: Word16
                        rounded = f16_sign .|. (fromIntegral (e16 .|. m16) + rounded_carry_in)

        f_negative :: Bool
        f_negative = testBit w32 31

        f16_infinity = f16_sign .|. f16_exp_mask
        f16_nan =  f16_sign .|. f16_exp_mask .|. f16_nan_mnt
          where f16_nan_mnt = fromIntegral (m32 `shiftR` (23 - 10)) :: Word16

        f16_sign :: Word16
        f16_sign = if f_negative then f16_sign_bit else 0
        -- w16_qnan = if testBit w32 22 then f16_qnan_bit else 0
        -- w16_nan_payload = if f32_payload_lo10 /= 0 then f32_payload_lo10 else 1
        --  where f32_payload_lo10 = fromIntegral (getBits 0 10 w32) :: Word16



halfToFloat :: Half -> Float
halfToFloat = halfBitsToFloat . halfToBits

halfBitsToFloat :: Word16 -> Float
halfBitsToFloat = bitsToFloat . halfBitsToFloatBits

halfBitsToFloatBits :: Word16 -> Word32
halfBitsToFloatBits w16 = f_bits
  where f_bits
          -- | trace (printf "s: 0x%x, e: 0x%x (0x%x), m: 0x%x" f32_s f32_exp (setF32 f32_exp) f32_mnt) False = undefined
          | f16_exp == f16_exp_mask && f16_mnt /= 0 = f32_sgn .|. f32_exp_mask .|. fromIntegral f32_mnt -- nan
          | f16_exp == f16_exp_mask = f32_sgn .|. f32_exp_mask
          | f16_exp == 0 && f16_mnt == 0 = f32_sgn -- +/- 0.0
          | f16_exp == 0 = normalize (127 - 15 + 1) f16_mnt
             -- trace (printf "%x %x" f32_exp f32_mnt) $ norm (127 - 15 + 1) f16_mnt
            -- f32_sgn .|. setF32 (f32_exp + 1) .|. (f32_mnt `shiftR` 1) -- den. 16 can be norm. 32

          | otherwise = f32_sgn .|. setF32 f32_exp .|. f32_mnt -- normal case
          where setF32 :: Int32 -> Word32
                setF32 f = (0x1FF .&. fromIntegral f32_exp) `shiftL` 23

                normalize :: Word32 -> Word16 -> Word32
                normalize e32 m16
                  | (m16 .&. min_exp) == 0 = normalize (e32 - 1) (2*m16)
                  | otherwise = f32_sgn .|. (e32`shiftL`23) .|. m16_shifted
                  where min_exp = fromIntegral f16_mnt_mask + 1
                        m16_shifted = fromIntegral m16`shiftL`(23 - 10) -- .&. f32_mnt_mask

        f16_sgn = f16_sign_bit .&. w16 :: Word16
        f16_exp = f16_exp_mask .&. w16 :: Word16
        f16_mnt = f16_mnt_mask .&. w16 :: Word16

        f32_sgn = fromIntegral f16_sgn `shiftL` 16 :: Word32
        f32_exp = f16_exp_val + 127 :: Int32
          where f16_exp_val = (fromIntegral f16_exp`shiftR`10) - 15
        f32_mnt = fromIntegral f16_mnt `shiftL` (23 - 10) :: Word32


