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

f64_MNT_BITS :: Int
f64_MNT_BITS = 52
f64_EXP_BITS :: Int
f64_EXP_BITS = 11
f64_SIGN_BIT :: Word64
f64_SIGN_BIT = 1`shiftL`(f64_MNT_BITS + f64_EXP_BITS)
f64_EXP_MASK :: Word64
f64_EXP_MASK = ((1`shiftL`f64_EXP_BITS) - 1)`shiftL`f64_MNT_BITS -- 0x7FF0000000000000
f64_QNAN_BIT :: Word64
f64_QNAN_BIT =  1`shiftL`(f64_MNT_BITS - 1) -- 0x0008000000000000
f64_MNT_MASK :: Word64
f64_MNT_MASK = (f64_QNAN_BIT`shiftL`1) - 1

f32_MNT_BITS :: Int
f32_MNT_BITS = 23
f32_EXP_BITS :: Int
f32_EXP_BITS = 8
f32_SIGN_BIT :: Word32
f32_SIGN_BIT = 1`shiftL`(f32_MNT_BITS + f32_EXP_BITS) -- 0x80000000
f32_EXP_MASK :: Word32
f32_EXP_MASK = ((1`shiftL`f32_EXP_BITS) - 1)`shiftL`f32_MNT_BITS -- 0x7F800000
f32_QNAN_BIT :: Word32
f32_QNAN_BIT = 1`shiftL`(f32_MNT_BITS - 1) -- 0x00400000
f32_MNT_MASK :: Word32
f32_MNT_MASK = (f32_QNAN_BIT`shiftL`1) - 1

f16_MNT_BITS :: Int
f16_MNT_BITS = 10
f16_EXP_BITS :: Int
f16_EXP_BITS = 5
f16_SIGN_BIT :: Word16
f16_SIGN_BIT = 1`shiftL`(f16_MNT_BITS + f16_EXP_BITS) -- 0x8000
f16_EXP_MASK :: Word16
f16_EXP_MASK = ((1`shiftL`f16_EXP_BITS) - 1)`shiftL`f16_MNT_BITS -- 0x7C00
f16_QNAN_BIT :: Word16
f16_QNAN_BIT = 1`shiftL`(f16_MNT_BITS - 1) -- 0x0200
f16_MNT_MASK :: Word16
f16_MNT_MASK = (f16_QNAN_BIT`shiftL`1) - 1 -- 0x03FF


-- the first fp32 value that converts to an overflow in fp16
-- (rounding can still cause overflow)
-- 0x47800000 = (143 = 16 + 127,0)
f32_F16_FIRST_OVERFLOW_U :: Word32
f32_F16_FIRST_OVERFLOW_U = (max_exp32 + max_exp16_p2)`shiftL`f32_MNT_BITS
  where max_exp16_p2 = 1`shiftL`(f16_EXP_BITS - 1) -- 16 = 14 + 1 + 1
        max_exp32 = (1`shiftL`(f32_EXP_BITS - 1)) - 1 -- 127

-- the lowest fp32 that will result in an normalized fp16 value
-- 0x38800000; exp is  113 which is 127 - 14 (f32_bias - f16_max_exp)
f32_F16_FIRST_NORM_U :: Word32
f32_F16_FIRST_NORM_U = (f32_bias - max_exp16)`shiftL`f32_MNT_BITS
  where f32_bias = (1`shiftL`(f32_EXP_BITS - 1)) - 1 -- 127
        -- 15 is all 1's (infinity), so 14 is the max fp16 exp
        max_exp16 = 1`shiftL`(f16_EXP_BITS - 1) - 1 - 1 -- 14 = 2^(5 exp bits - 1) - 1 - 1

-- 0x33000000 = (102 = 127 - 15 - 10)
--   -15 is for fp16 rebias, -10 for the 10 mantissa fp16 bits we can
--   trade to increment the exponent; i.e. if the exp underflows,
--   we can increment and scale the mantissa by 2
f32_F16_FIRST_DENORM_U :: Word32
f32_F16_FIRST_DENORM_U = (f32_bias - f16_bias - f16_mnt_bits)`shiftL`f32_MNT_BITS
  where f32_bias = (1`shiftL`(f32_EXP_BITS - 1)) - 1 -- 127
        f16_bias = 1`shiftL`(f16_EXP_BITS - 1) - 1 -- -15
        f16_mnt_bits = fromIntegral f16_MNT_BITS -- -10

------------------------------
-- c.f. f32_F16_....
--
-- 0x40F0000000000000
--   1039 = 1023 + 16
f64_F16_FIRST_OVERFLOW_U :: Word64
f64_F16_FIRST_OVERFLOW_U = (bias64 + max_exp16_p2)`shiftL`f64_MNT_BITS
   where max_exp16_p2 = 1`shiftL`(f16_EXP_BITS - 1) -- 16 = 14 + 1 + 1
         bias64 = (1`shiftL`(f64_EXP_BITS - 1)) - 1 -- 1023

------------------------------
-- c.f. f32_F16_....
--
-- 0x3F10000000000000
--   1009 = 1023 - 14
f64_F16_FIRST_NORM_U :: Word64
f64_F16_FIRST_NORM_U = (f64_bias - max_exp16)`shiftL`f64_MNT_BITS
   where f64_bias = (1`shiftL`(f64_EXP_BITS - 1)) - 1 -- 1023
         -- 15 is all 1's (infinity), so 14 is the max fp16 exp
         max_exp16 = 1`shiftL`(f16_EXP_BITS - 1) - 1 - 1 -- 14 = 2^(5 exp bits - 1) - 1 - 1


------------------------------
-- c.f. f32_F16_....
--
-- 0x3E60000000000000
--   998 = 1023 - 15 - 10
f64_F16_FIRST_DENORM_U :: Word64
f64_F16_FIRST_DENORM_U = (f64_bias - f16_bias - f16_mnt_bits)`shiftL`f64_MNT_BITS
  where f64_bias = (1`shiftL`(f64_EXP_BITS - 1)) - 1 -- 1023
        f16_bias = 1`shiftL`(f16_EXP_BITS - 1) - 1 -- -15
        f16_mnt_bits = fromIntegral f16_MNT_BITS -- -10


------------------------------------------------------------------------------
-- deals with NaN
floatToDouble :: Float -> Double
floatToDouble f32
  | f32 == f32 = float2Double f32
  | otherwise = bitsToDouble (w64_sign .|. w64_inf_exp .|. w64_qnan .|. w64_payload)
  where w32 = floatToBits f32
        w64_sign = if testBit w32 31 then f64_SIGN_BIT else 0
        w64_qnan = if testBit w32 22 then f64_QNAN_BIT else 0
        w64_inf_exp = 0x7FF0000000000000
        -- following IGA rules here, use the bottom for the payload
        w64_payload = fromIntegral $ getBits 0 22 w32

-- deals with NaN
doubleToFloat :: Double -> Float
doubleToFloat f64
  | f64 == f64 = double2Float f64
  | otherwise = bitsToFloat (w32_sign .|. w32_exp_inf .|. w32_qnan .|. w32_nan_payload)
  where w64 = doubleToBits f64
        w32_sign = if testBit w64 63 then f32_SIGN_BIT else 0
        w32_qnan = if testBit w64 51 then f32_QNAN_BIT else 0
        w32_exp_inf = f32_EXP_MASK
        w32_nan_payload = if f64_payload_lo22 /= 0 then f64_payload_lo22 else 1
            where f64_payload_lo22 = fromIntegral (getBits 0 22 w64) :: Word32

getBits off len val = ((val `shiftR` off) .&. ((1`shiftL`len) - 1))

-- deals with NaN
negateDouble :: Double -> Double
negateDouble d
  | d == d = negate d
  | otherwise = bitsToDouble w64_negated
  where w64_negated = doubleToBits d `xor` f64_SIGN_BIT

-- http://www.open-std.org/jtc1/sc22/wg14/www/docs/n2016.pdf
newtype Half = Half Word16

halfToBits :: Half -> Word16
halfToBits (Half w16) = w16
halfFromBits :: Word16 -> Half
halfFromBits = Half

instance Show Half where
--  -- http://www.open-std.org/jtc1/sc22/wg14/www/docs/n2016.pdf
--  -- 5.1 uses "sf" (h would be better)
--  show = (++"sf") . show . halfToFloat
-- show = show . halfToFloat
  show = fmtHalf

fmtHalf :: Half -> String
fmtHalf h@(Half bits)
  | isNaN h = nan_str
  | otherwise = show (halfToFloat h)
  where nan_str
          | (f16_QNAN_BIT .&. bits) == f16_QNAN_BIT = printf "qnan(0x%03X)" nan_payload
          | otherwise = printf "snan(0x%03X)" nan_payload
          where nan_payload =
                  (f16_MNT_MASK .&. complement f16_QNAN_BIT) .&. bits


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

instance Num Half where
  (+) = hlift2Repack (+)
  (-) = hlift2Repack (-)
  (*) = hlift2Repack (*)
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


instance Fractional Half where
  -- (/) :: a -> a -> a
  (/) = hlift2Repack (/)

  -- recip :: a -> a
  recip = floatToHalf . recip . halfToFloat

  -- fromRational :: Rational -> a
  fromRational = floatToHalf . fromRational


instance Floating Half where
  --   pi :: a
  pi = floatToHalf pi

  -- exp :: a -> a
  exp = hliftRepack exp

  -- log :: a -> a
  log = hliftRepack log

  -- sqrt :: a -> a
  sqrt = hliftRepack sqrt

  -- (**) :: a -> a -> a
  (**) = hlift2Repack (**)

  -- logBase :: a -> a -> a
  logBase = hlift2Repack logBase

  -- sin :: a -> a
  sin = hliftRepack sin

  -- cos :: a -> a
  cos = hliftRepack cos

  -- tan :: a -> a
  tan = hliftRepack tan

  -- asin :: a -> a
  asin = hliftRepack asin

  -- acos :: a -> a
  acos = hliftRepack acos

  -- atan :: a -> a
  atan = hliftRepack atan

  -- sinh :: a -> a
  sinh = hliftRepack sinh

  -- cosh :: a -> a
  cosh = hliftRepack cosh

  -- tanh :: a -> a
  tanh = hliftRepack tanh

  -- asinh :: a -> a
  asinh = hliftRepack asinh

  -- acosh :: a -> a
  acosh = hliftRepack acosh

  -- atanh :: a -> a
  atanh = hliftRepack atanh


instance RealFrac Half where
  -- properFraction :: Integral b => a -> (b, a)
  properFraction h =
    case properFraction (halfToFloat h) of
      (b,f) -> (b,floatToHalf f)

  -- truncate :: Integral b => a -> b
  truncate = truncate . halfToFloat

  -- round :: Integral b => a -> b
  round = round . halfToFloat

  -- ceiling :: Integral b => a -> b
  ceiling = ceiling . halfToFloat

  -- floor :: Inegral b => a -> b
  floor = floor . halfToFloat


instance RealFloat Half where
  -- a constant function, returning the radix of the representation (often 2)
  floatRadix _ = 2

  -- a constant function, returning the number of digits of floatRadix in the significand
  floatDigits _  = 11 -- 24 == 23 + 1 for Float (hidden one)

  -- a constant function, returning the lowest and highest values the exponent may assume
  -- floatRange :: a -> (Int, Int)
  floatRange _ = (-14,16) -- (-125,128) for Float

  -- The function decodeFloat applied to a real floating-point number returns
  -- the significand expressed as an Integer and an appropriately scaled
  -- exponent (an Int). If decodeFloat x yields (m,n), then x is equal in value
  -- to m*b^^n, where b is the floating-point radix, and furthermore, either m and n
  -- are both zero or else b^(d-1) <= abs m < b^d, where d is the value of floatDigits x.
  -- In particular, decodeFloat 0 = (0,0). If the type contains a negative zero,
  -- also decodeFloat (-0.0) = (0,0). The result of decodeFloat x is unspecified if
  -- either of isNaN x or isInfinite x is True.
  --
  -- decodeFloat :: a -> (Integer, Int)
  decodeFloat _ = error "decodeFloat<Half>: not implemented"
  -- decodeFloat

  -- encodeFloat performs the inverse of decodeFloat in the sense that for finite x
  -- with the exception of -0.0, uncurry encodeFloat (decodeFloat x) = x.  encodeFloat m n
  -- is one of the two closest representable floating-point numbers to m*b^^n
  -- (or +/-Infinity if overflow occurs); usually the closer, but if m contains too many
  -- bits, the result may be rounded in the wrong direction.
  --
  -- encodeFloat :: Integer -> Int -> a
  -- encodeFloat _ _ = error "encodeFloat<Half>: not implemented"
  encodeFloat s e = floatToHalf (encodeFloat s e)

  -- exponent corresponds to the second component of decodeFloat.  exponent 0 = 0 and
  -- for finite nonzero x, exponent x = snd (decodeFloat x) + floatDigits x.  If x is a
  -- finite floating-point number, it is equal in value to significand x * b ^^ exponent x,
  -- where b is the floating-point radix. The behaviour is unspecified on infinite
  -- or NaN values.
  --
  -- *** same as std::frexp return param ***
  --
  -- exponent :: a -> Int
  -- exponent _ = error "exponent<Half>: not implemented"
  exponent = exponent . halfToFloat


  -- The first component of decodeFloat, scaled to lie in the open interval (-1,1), either 0.0 or of absolute value >= 1/b, where b is the floating-point radix. The behaviour is unspecified on infinite or NaN values.
  --
  -- *** same as std::frexp return value ***
  --
  -- significand :: a -> a
  significand _ = error "significand<Half>: not implemented"

  -- multiplies a floating-point number by an integer power of the radix
  --
  -- scaleFloat :: Int -> a -> a
  scaleFloat s h = floatToHalf (scaleFloat s (halfToFloat h))

  -- isNaN :: a -> Bool
  isNaN (Half bits) =
    (bits .&. f16_EXP_MASK) == f16_EXP_MASK &&
      (bits .&. f16_MNT_MASK) /= 0

  -- isInfinite :: a -> Bool
  isInfinite (Half bits) =
    (bits .&. f16_EXP_MASK) == f16_EXP_MASK &&
      (bits .&. f16_MNT_MASK) == 0

  -- isDenormalized :: a -> Bool
  isDenormalized (Half bits) =
    (bits .&. f16_EXP_MASK) == 0 &&
      (bits .&. f16_MNT_MASK) > 0

  -- isNegativeZero :: a -> Bool
  isNegativeZero (Half bits) = bits == 0x8000

  -- isIEEE :: a -> Bool
  isIEEE _ = True

  -- atan2 :: a -> a -> a
  atan2 = hlift2Repack atan2


-- instance RealFloat Float where -- Defined in `GHC.Float'
-- instance RealFrac Float where -- Defined in `GHC.Float'

-- hlift :: (Float -> a) -> Half -> a
-- hlift = halfToFloat

hliftRepack :: (Float -> Float) -> Half -> Half
hliftRepack func = floatToHalf . func . halfToFloat


hlift2 :: (Float -> Float -> a) -> Half -> Half -> a
hlift2 func h1 h2 = func (halfToFloat h1) (halfToFloat h2)


hlift2Repack :: (Float -> Float -> Float) -> Half -> Half -> Half
hlift2Repack func = hlift2 (\h1 -> floatToHalf . func h1)

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

-- mostly ported from half.hpp
floatBitsToHalfBits :: Round -> Word32 -> Word16
floatBitsToHalfBits r w32
  | w32_u > f32_EXP_MASK = f16_nan -- +/- nan
  | w32_u == f32_EXP_MASK = f16_sign .|. f16_EXP_MASK -- +/-infinity
  | w32_u >= f32_F16_FIRST_OVERFLOW_U = overflow
  | w32_u >= f32_F16_FIRST_NORM_U = norm
  | w32_u >= f32_F16_FIRST_DENORM_U = denorm
  | w32_u /= 0 = underflow
  | otherwise = f16_sign -- +/- 0.0
  where w32_u :: Word32
        w32_u = w32 .&. complement f32_SIGN_BIT

        f16_sign32 :: Word32
        f16_sign32 = (w32`shiftR`16) .&. 0x8000
        f16_sign :: Word16
        f16_sign = fromIntegral f16_sign32

        sgn x = if x /= 0 then 1 else 0

        bias_diff = 127 - 15
        mnt_diff = 23 - 10

        f16_nan :: Word16
        f16_nan =  f16_sign .|. f16_EXP_MASK .|. f16_nan_mnt .|. ensure_nonzero_mantissa
          where f16_nan_mnt = fromIntegral $
                    ((w32 .&. f32_MNT_MASK)`shiftR`mnt_diff) :: Word16
                ensure_nonzero_mantissa = if f16_nan_mnt == 0 then 1 else 0

        norm :: Word16
        norm = round (f16_sign32 .|. v) g s
          where v = (((w32_u`shiftR`23) - bias_diff)`shiftL`10) .|.
                      ((w32_u`shiftR`13) .&. fromIntegral f16_mnt_mask)
                g = (w32_u`shiftR`12) .&. 1
                s = sgn (w32_u .&. 0x0FFF)

        denorm :: Word16
        denorm = round v g s
          where i = 125 - fromIntegral (w32_u`shiftR`23)
                w32_u2 = (w32_u .&. 0x007FFFFF) .|. 0x00800000
                v = f16_sign32 .|. (w32_u2`shiftR`(i+1))
                g = (w32_u2`shiftR`i) .&. 1
                s = sgn (w32_u2 .&. ((1`shiftL`i)-1))

        overflow :: Word16
        overflow =
          case r of
            RoundE -> f16_sign .|. 0x7C00
            RoundP -> f16_sign + 0x7C00 - (f16_sign`shiftR`15)
            RoundN -> f16_sign + 0x7BFF + (f16_sign`shiftR`15)
            RoundZ -> f16_sign .|. 0x7BFF

        underflow :: Word16
        underflow =
            -- would include underflow flag
          case r of
            RoundP -> f16_sign + 1 - (f16_sign`shiftR`15)
            RoundN -> f16_sign +     (f16_sign`shiftR`15)
            _ -> f16_sign

        -- v - value finite half-precision number to round
        -- g - guard bit (most significant discarded bit)
        -- s - sticky bit (or of all but the most significant discarded bits)
        round :: Word32 -> Word32 -> Word32 -> Word16
        round v g s = fromIntegral $
          case r of
            RoundE -> v + (g .&. (s .|. v))
            RoundP -> v + (complement (v`shiftR`15) .&. (g .|. s))
            RoundN -> v +            ((v`shiftR`15) .&. (g .|. s))
            RoundZ -> v


halfToFloat :: Half -> Float
halfToFloat = halfBitsToFloat . halfToBits

halfBitsToFloat :: Word16 -> Float
halfBitsToFloat = bitsToFloat . halfBitsToFloatBits

halfBitsToFloatBits :: Word16 -> Word32
halfBitsToFloatBits w16 = f_bits
  where f_bits
          -- | trace (printf "s: 0x%x, e: 0x%x (0x%x), m: 0x%x" f32_s f32_exp (setF32 f32_exp) f32_mnt) False = undefined
          | f16_exp == f16_exp_mask && f16_mnt /= 0 = f32_sgn .|. f32_EXP_MASK .|. fromIntegral f32_mnt -- nan
          | f16_exp == f16_exp_mask = f32_sgn .|. f32_EXP_MASK
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
                        m16_shifted = fromIntegral m16`shiftL`(23 - 10) -- .&. f32_MNT_MASK

        f16_sgn = f16_sign_bit .&. w16 :: Word16
        f16_exp = f16_exp_mask .&. w16 :: Word16
        f16_mnt = f16_mnt_mask .&. w16 :: Word16

        f32_sgn = fromIntegral f16_sgn `shiftL` 16 :: Word32
        f32_exp = f16_exp_val + 127 :: Int32
          where f16_exp_val = (fromIntegral f16_exp`shiftR`10) - 15
        f32_mnt = fromIntegral f16_mnt `shiftL` (23 - 10) :: Word32



---------------------------
-- unused

normalFloatToHalfRE :: Word32 -> Word16
normalFloatToHalfRE w32
  -- assume not a special case: NaN, infinity, 0, etc ...
  | e32 > f32_F16_BIAS_DIFF + f16_max_exp = f16_sign .|. f16_exp_mask -- overflows to infinity
  | e32 < e32_MIN_REPRESENTIBLE = f16_sign -- rounds down to 0.0
  | e32 >= e32_MIN_REPRESENTIBLE && e32 <= f32_F16_BIAS_DIFF =
    fixDenormLoop e32 (m32 .|. m32_hidden_one)
  | otherwise = error "normal case with rounding"
  where m32 = w32 .&. f32_MNT_MASK
        e32 = (w32 .&. f32_EXP_MASK) `shiftR` 23
        f16_nan =  f16_sign .|. f16_exp_mask .|. f16_nan_mnt
          where f16_nan_mnt = fromIntegral (m32 `shiftR` (23 - 10)) :: Word16

        f32_F16_BIAS_DIFF = 127 - 15 -- 112

        -- -10 is the 10 mantissa bits that we can shift into (23 - 10)
        e32_MIN_REPRESENTIBLE = (127 - 15) - 10 -- 102 (0x66)

        -- 0x1E -- (2^5-2); this is -2 because 0x1F is infinity
        f16_max_exp = ((1`shiftL`5) - 1 - 1)

        m32_hidden_one = f32_QNAN_BIT`shiftL`1

        f16_sign = fromIntegral (w32 `shiftR` 16) :: Word16

        fixDenormLoop :: Word32 -> Word32 -> Word16
        fixDenormLoop e32 m32
          | trace dbg False = undefined
          | e32 <= f32_F16_BIAS_DIFF = fixDenormLoop (e32+1) (m32`shiftR`1)
          -- Now we have a zero fp16 exponent but some mantissa bits leftover
          | otherwise = f16_sign .|. fromIntegral (m32`shiftR`(f32_MNT_BITS - f16_MNT_BITS))
          where dbg :: String
                dbg =
                  printf "e32:%02X  m32:%06X" e32 m32


roundNearestEven :: Word32 -> Word32 -> Word32 -> Word16
roundNearestEven w32 e16 m32 =
    -- the rounding carry in bit is 1 or 0 and
    --   added to the new mantissa (potentially incrementing it)
    --   if mnt overflows it will overflow into exp (mnt will be 0's in this case)
    --   if exp saturates to all 1's that will be a clean infinity pattern
    --     (exp cannot overflow to 0 since caller would have caught that
    --     as infinity earlier)
    f16_sign .|. (fromIntegral (e16 .|. m16) + rounded_carry_in)
  where m16 = m32`shiftR`(23 - 10)
        -- fp32: S`EEEEEEEE`MMMMMMMMMMMMMMMMMMMMMMM
        -- fp16:    s`eeeee`mmmmmmmmmm|||||||||||||
        --                            ^ roundoff  ^
        -- bits rounded off
        m32_roundoff = (m32 .&. ((1`shiftL`(23 - 10)) - 1))
        -- bit 12... (23 - 10) is the high roundoff
        rounded_carry_in :: Word16
        rounded_carry_in
          -- closer to 0 (e.g. decimal YY.XX4...)
          | m32_roundoff < 0x1000 = 0
          -- closer to 1 (e.g. decimal YY.XX6...)
          | m32_roundoff > 0x1000 = 1
          -- ties to even (e.g. decimal YY.XX5...)
          | otherwise = if odd m16 then 1 else 0

        f16_sign :: Word16
        f16_sign = fromIntegral (w32 `shiftR` 16)


quantizeMantissaRNE :: Int -> Word32 -> Word32
quantizeMantissaRNE mnt_len w32
  | (w32 .&. f32_EXP_MASK) == f32_EXP_MASK = w32 -- inf/nan don't change
  | otherwise = quantized_result
  where m32 = w32 .&. f32_MNT_MASK
        e32 = (w32 .&. f32_EXP_MASK) `shiftR` 23

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




f16_sign_bit :: Word16
f16_sign_bit = 0x8000
f16_exp_mask :: Word16
f16_exp_mask = 0x7C00
f16_qnan_bit :: Word16
f16_qnan_bit = 0x0200
f16_mnt_mask :: Word16
f16_mnt_mask = (f16_qnan_bit`shiftL`1) - 1


floatBitsToHalfBitsBROKEN :: Round -> Word32 -> Word16
floatBitsToHalfBitsBROKEN r w32_unquant
  | (w32 .&. complement f32_SIGN_BIT) == 0 = f16_sign -- +/-0.0
  | (w32 .&. f32_EXP_MASK) == f32_EXP_MASK = f16_infinity -- +/-infinity
  | (w32 .&. f32_EXP_MASK) == f32_EXP_MASK && m32 /= 0 = f16_nan -- nan
  | otherwise = non_nan -- normal or subnormal
  where m32 = w32 .&. f32_MNT_MASK
        e32 = (w32 .&. f32_EXP_MASK) `shiftR` 23

        w32
          -- | r == RoundE = quantizeMantissaRNE 11 w32_unquant
          | otherwise = w32_unquant

        f32_mnt_bits = 23
        f16_mnt_bits = 10
        mnt_diff = f32_mnt_bits - f16_mnt_bits

        -- subtracting this from the exponent takes an exp from the domain of
        -- fp32 to fp16
        f32_f16_bias_diff = 127 - 15 -- 112
        -- -10 is the 10 mantissa bits that we can shift into (23 - 10)
        e32_min_representible = (127 - 15) - 10 -- 102 (0x66)
        f16_max_exp = ((1`shiftL`5) - 1 - 1) -- 0x1E -- (2^5-2); this is -2 because 0x1F is infinity

        non_nan
          -- TODO: remove later (redirect for now)
          | r == RoundE = normalFloatToHalfRE w32
          --
          -- e16 overflows fp16 exponent after bias fixup ==> +/-INFINITY
          | e32 > f32_f16_bias_diff + f16_max_exp = f16_infinity
          --
          -- too small: rounds to +/-0
          | e32 < e32_min_representible = f16_sign
          --
--          | r == RoundE = round_even_non_nan

          -- denorm value
          --   0x66 is (127 - 15) - 10 (10b of mantissa we can use in fp16)
          --  (127 - 15) - 10 <= e32 <= (127 - 15)
          --
          -- fp32 would underflow to 0, but we can use a denorm fp16 by
          -- trading mantissa bits (shift right) for exponent range (incrementing exp)
          | e32 >= e32_min_representible && e32 <= f32_f16_bias_diff =
            -- trace ("DEN: " ++ printf "e32:%08X  m32:%08X" e32 m32) $
              fixDenormLoop e32 (m32 .|. m32_hidden_one)
          --
          -- normalized value
          | otherwise =
            completeNonNan
              ((e32 - f32_f16_bias_diff)`shiftL`f16_mnt_bits)
              m32
          where fixDenormLoop e32 m32
                  -- | trace dbg False = undefined
                  | e32 <= f32_f16_bias_diff = fixDenormLoop (e32+1) (m32`shiftR`1)
                  -- | otherwise = completeNonNan 0 m32
                  --
                  -- Now we have a zero fp16 exponent but some mantissa bits leftover
                  | otherwise = f16_sign .|. fromIntegral (m32`shiftR`(f32_mnt_bits - f16_mnt_bits))
                  where dbg :: String
                        dbg =
                          printf "e32:%02X  m32:%06X" e32 m32

                m32_hidden_one = f32_QNAN_BIT`shiftL`1

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
                        m32_roundoff = (m32 .&. ((1`shiftL`mnt_diff) - 1))
                        -- bit 12... (23 - 10) is the high roundoff
                        rounded_carry_in :: Word16
                        rounded_carry_in
                          | m32_roundoff == 0 = 0
                          | otherwise =
                            case r of
                              RoundE
                                -- closer to 0 (e.g. decimal YY.XX4...)
                                | m32_roundoff < 0x1000 -> 0
                                -- closer to 1 (e.g. decimal YY.XX6...)
                                | m32_roundoff > 0x1000 -> 1
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

