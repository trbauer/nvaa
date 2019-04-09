module NVT.Floats where

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

doubleToFloat :: Double -> Float
doubleToFloat = double2Float -- realToFrac
floatToDouble :: Float -> Double
floatToDouble = float2Double -- realToFrac
