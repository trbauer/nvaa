module NVT.Floats where

import Data.Bits
import Data.Int
import Data.Word
import Unsafe.Coerce(unsafeCoerce)
import GHC.Float



doubleToBits :: Double -> Word64
doubleToBits = unsafeCoerce
floatToBits :: Float -> Word32
floatToBits = unsafeCoerce

bitsToDouble :: Word64 -> Double
bitsToDouble = unsafeCoerce
bitsToFloat :: Word32 -> Float
bitsToFloat = unsafeCoerce

-- doubleToFloat :: Double -> Float
-- doubleToFloat = double2Float -- realToFrac
-- floatToDouble :: Float -> Double
-- floatToDouble = float2Double -- realToFrac

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

f64_sign_bit :: Word64
f64_sign_bit = 0x8000000000000000
-- f64_exp_mask :: Word64
-- f64_exp_mask = 0x7FF0000000000000
f64_qnan_bit :: Word64
f64_qnan_bit = 0x0008000000000000

f32_sign_bit :: Word32
f32_sign_bit = 0x80000000
f32_exp_mask :: Word32
f32_exp_mask = 0x7F800000
f32_qnan_bit :: Word32
f32_qnan_bit = 0x00400000

f16_sign_bit :: Word16
f16_sign_bit = 0x8000
f16_exp_mask :: Word16
f16_exp_mask = 0x7C00
f16_qnan_bit :: Word16
f16_qnan_bit = 0x0200
f16_mnt_mask :: Word16
f16_mnt_mask = f16_qnan_bit - 1

-- deals with NaN
doubleToFloat :: Double -> Float
doubleToFloat f64
  | f64 == f64 = double2Float f64
  | otherwise = bitsToFloat (w32_sign .|. w32_exp_inf .|. w32_qnan .|. w32_payload)
  where w64 = doubleToBits f64
        w32_sign = if testBit w64 63 then f32_sign_bit else 0
        w32_qnan = if testBit w64 51 then f32_qnan_bit else 0
        w32_exp_inf = 0x7F800000
        w32_payload = if f64_payload_lo22 /= 0 then f64_payload_lo22 else 1
            where f64_payload_lo22 = fromIntegral (getBits 0 22 w64) :: Word32

getBits off len val = ((val `shiftR` off) .&. ((1`shiftL`len) - 1))

-- deals with NaN
negateDouble :: Double -> Double
negateDouble d
  | d == d = negate d
  | otherwise = bitsToDouble w64
  where w64 = doubleToBits d `xor` f64_sign_bit


floatToHalf :: Float -> Word16
floatToHalf f32
  | abs f32 == 1/0 = w16_sign .|. f16_exp_mask -- +/-infinity
  | f32 == 0.0 = w16_sign -- +-0.0hf
  | f32 == f32 = error "floatToHalf not implemented for normal values" -- normal value
  | otherwise = w16_sign .|. w16_qnan .|. w16_payload -- nan
  where w32 = floatToBits f32

        w16_sign = if testBit w32 31 then f16_sign_bit else 0
        w16_qnan = if testBit w32 22 then f16_qnan_bit else 0
        w16_payload = if f32_payload_lo10 /= 0 then f32_payload_lo10 else 1
          where f32_payload_lo10 = fromIntegral (getBits 0 10 w32) :: Word16


halfToFloat :: Word16 -> Float
halfToFloat w16 = bitsToFloat f_bits
  where f_bits
          -- | trace (printf "s: 0x%x, e: 0x%x (0x%x), m: 0x%x" f32_s f32_exp (setF32 f32_exp) f32_mnt) False = undefined
          | f16_exp == f16_exp_mask && f16_mnt /= 0 = f32_sgn .|. f32_exp_mask .|. fromIntegral f32_mnt -- nan
          | f16_exp == 0 && f16_mnt == 0 = f32_sgn -- +/- 0.0
          | f16_exp == 0 = f32_sgn .|. setF32 (f32_exp+1) .|. (f32_mnt `shiftR` 1) -- den. 16 can be norm. 32
          | otherwise = f32_sgn .|. setF32 f32_exp .|. f32_mnt -- normal case
          where setF32 :: Int32 -> Word32
                setF32 f = (0x1FF .&. fromIntegral f32_exp) `shiftL` 23

        f16_sgn = w16 .&. 0x8000
        f16_exp = f16_exp_mask .&. w16 :: Word16
        f16_mnt = f16_mnt_mask .&. w16 :: Word16

        f32_sgn :: Word32
        f32_sgn = fromIntegral f16_sgn `shiftL` 16

        f32_exp = f16_exp_val + 127
          where f16_exp_val = (fromIntegral f16_exp`shiftR`10) - 15 :: Int32
        f32_mnt = fromIntegral f16_mnt `shiftL` (23 - 10) :: Word32

