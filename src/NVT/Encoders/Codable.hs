module NVT.Encoders.Codable where

import Data.Word

class Codeable c where
  {-# MINIMAL decode, encode #-}
  decode :: Word64 -> Either String c
  encode :: c -> Either String Word64
  

encodeEnum :: Enum c => c -> Either String Word64
encodeEnum = Right . fromIntegral . fromEnum 

encodeEnumX :: (Eq c,Enum c,Show c) => [c] -> c -> Either String Word64
encodeEnumX exceptions c
  | c`elem`exceptions = Left (show c ++ ": cannot encode value")
  | otherwise = Right (fromIntegral (fromEnum c))

decodeEnum :: Enum c => c -> c -> Word64 -> Either String c
decodeEnum lo hi w
  | w < fe lo || w > fe hi = Left "value out of bounds"
  | otherwise = Right (toEnum (fromIntegral w))
  where fe = fromIntegral . fromEnum

instance Codeable Bool where
  decode 0 = Right False
  decode 1 = Right True
  decode _ = Left "invalid bool"
  --
  encode False = Right 0
  encode True = Right 1


instance Codeable Word32 where
  -- decode :: Word64 -> Either String c
  decode w
    | w > 0xFFFFFFFF = Left "value out of bounds"
    | otherwise  = Right $ fromIntegral w
  -- encode :: c -> Either String Word64
  encode = Right . fromIntegral




class Syntax s where
  format :: s -> String

