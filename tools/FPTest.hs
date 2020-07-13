{-# LANGUAGE MultiParamTypeClasses, BangPatterns #-}
module Main where

import NVT.Floats
import NVT.Bits

import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Word
import Debug.Trace
import System.Directory
import System.Exit
import System.Environment
import System.Process
import Text.Printf
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- import Data.Array.IArray
-- import Data.Array.Unboxed

-- type Word16Array =
--  IArray UArray Word16

-- OPENS:
--    - understand RTZ when "25.6"
--    - simplifiy overflow doesn't need a special pattern!
--          ==> mantissa will be 0's; exp will be saturated (sign bit or's in)

--    - ensure RTE is correct (test overflow case?)
--       0x7BFF = 0`11110`1111111111 ==> 0x477FE000 (max positive FP16)
--          should not overflow?
--            NO  0x477FEFFF => 0x7BFF -- no overflow (closer to 0)
--            YES 0x477FF000 => 0x7C00 -- overflows on tie and even
--            YES 0x477FF001 => 0x7C00 -- overflows (closer to 1)
--          so 0x477FF000 should cause the overflow case
runTests :: IO ()
runTests = runTestCases rounding_tests


runTestCases :: [(Round,Word32,Word16)] -> IO ()
runTestCases cs = do
  mapM_ (uncurry3 testFromF32) cs

uncurry3 f = \(x,y,z) -> f x y z

rounding_tests :: [(Round,Word32,Word16)]
rounding_tests =
    concat [
      tA 0 0
      -- roundoff is closer to zero
--      tA 0x477FEFFF 0x7BFF -- [all modes] no overflow (closer to 0)
      -- roundoff exactly ties

     -- RTE with edges of the number line
    , te 0x477FF000 0x7C00 -- +1: low digit is odd
    , te 0xC77FF000 0xFC00 -- +1: low digit is odd
    , te 0x477FE000 0x7BFF -- +0: low digit is even
    , te 0xC77FE000 0xFBFF -- +0: low digit is even
--    , tp 0x477FF000 0x7C00 -- +1: rounding towards positive infinity
--    , tp 0xC77FF000 0xFC00 -- +0: rounding towards positive infinity
--    , tn 0xC77FF000 0xFC00 -- +1: rounding towards negative infinity
      -- roundoff closer to one
--    , tA 0x477FF001 0x7C00 -- RTE         overflow (closer to 1)
--    , tn 0x33000800  0x0000
--    , tp 0x33000800  0x0001
    ]
  where tA x y = [(r,x,y)| r<-[RoundZ,RoundE,RoundP,RoundN]]
        te x y = [(RoundE,x,y)]
        tn x y = [(RoundN,x,y)]
        tp x y = [(RoundP,x,y)]
        tz x y = [(RoundZ,x,y)]

main :: IO ()
main = do
  as <- getArgs
  let exitOn x = if x == 0 then exitSuccess else exitFailure
  case as of
    [] -> do
      errs1 <- testAll32 RoundE
      errs2 <- testAll32 RoundN
      errs3 <- testAll32 RoundP
      errs4 <- testAll32 RoundZ
      exitOn (errs1 + errs2 + errs3 + errs4)
    ["e"] -> testAll32 RoundE >>= exitOn
    ["n"] -> testAll32 RoundN >>= exitOn
    ["p"] -> testAll32 RoundP >>= exitOn
    ["z"] -> testAll32 RoundZ >>= exitOn
    _ -> die "usage: fptest.exe (e|n|p|z)"

testAll32 :: Round -> IO Int
testAll32 r = do
  putStrLn $ "****************** testing " ++ show r
  errs <- sum <$> mapM (testAll32Chunk r) [0x0 .. 0xF]
  unless (errs == 0) $
    putStrLn $ show errs ++ " errors"
  when (errs == 0) $
    putStrLn $ "rounding method validated"
  return errs

testAll32Chunk :: Round -> Word32 -> IO Int
testAll32Chunk r w32_hi = do
    putStrLn $ printf "**** testing range 0x%X" (w32_hi`shiftR`28)
    lbs <- mkData r w32_hi
    run (0 :: Int) 0x0 0x0 lbs <* removeFile dat_file
  where dat_file = mkF r w32_hi
        run :: Int -> Word32 -> Int -> LBS.ByteString -> IO Int
        run !errs w32_lo off lbs = do
          when ((w32_lo .&. 0x00FFFFFF) == 0x00FFFFFF) $
            -- the high nibble is implied
            -- the second highest nibble is the counter
            putStrLn $ "... " ++ show (0x0F - (w32_lo`shiftR`24)) ++ " left"
          case LBS.splitAt 2 lbs of
            (lbs_w2,lbs_sfx) -> do
              let w16_ref = fromByteStringU16LE (LBS.toStrict lbs_w2)
              let w32 = (w32_hi`shiftL`28) .|. w32_lo
              z <- testFromF32 r w32 w16_ref
              let errs1 = if z then errs else (errs+1)
              when (errs1 > 4) $ do
                putStrLn $ ">>>>>>>>>>>>>>>>>> " ++ dat_file ++ ":" ++ printf "%08X: stopping to max errors" off
                exitFailure
              if w32_lo == 0x0FFFFFFF then do
                  unless (errs == 0) $
                    putStrLn $ show errs ++ " errors"
                  if LBS.null lbs then do
                      putStrLn $ "ERROR: extra bits at end of stream"
                      return (errs+1)
                    else return errs
                else do
                  run errs1 (w32_lo+1) (off+2) lbs_sfx

mkF :: Round -> Word32 -> FilePath
mkF r w32_hi = "half-rt" ++ roundSuffix r ++ "-" ++ printf "%XXXXXXXX" w32_hi ++ ".dat"

mkData :: Round -> Word32 -> IO LBS.ByteString
mkData r w32_hi = do
  let krn = "all_f16s_rt" ++ roundSuffix r
  let n = 2^32`div`0xF
  let dat_file = mkF r w32_hi
  putStrLn $ "************ generating " ++ dat_file
  let cls_expr =
        "let A=0:w; " ++
        "#1`tools/halfgen.bin`all_f16s_rt" ++
          roundSuffix r ++ "<" ++ show n ++ ">(" ++
          "A," ++ printf "0x%08X" w32_hi ++ "); " ++
        "save('" ++ dat_file ++ "',A);"
  out <- readProcess "cls64.exe" ["-e", cls_expr] ""
  -- putStr out
  LBS.readFile dat_file

-- if roundoff (>=0x1000)
--  0xBAEB9000   0x975C
--  0x3E1D1000   0x30E8
--  0x3D781000   0x2BC0
--  0x3B825000   0x1C12
--  0x3AF89000   0x17C4
-- if roundoff (>0x1000)
--  0xBB75B000   0x9BAE    0x9BAD
--  0x39E07000   0x0F04
--  0x3A413000   0x120A
--  0x3E77F000   0x33C0
--  0x3DC6B000   0x2E36
cheat :: Round -> Word32 -> IO ()
cheat r w32 = do
  putStrLn $ show r ++ ":"
  putStrLn $ "F32: " ++ fmtF32 (floatFromBits w32)
  putStrLn $ "SUT: " ++ fmtF16 (halfFromBits (floatBitsToHalfBits r w32))
  w16 <- hwToF16 r w32
  putStrLn $ "GPU: " ++ fmtF16 (halfFromBits w16)

hwToF16 :: Round -> Word32 -> IO Word16
hwToF16 r w32 = (\[x] -> x) <$> hwToF16s 10000 r [w32]
hwToF16s :: Int -> Round -> [Word32] -> IO [Word16]
hwToF16s ix r w32s = do
  let in_file = "fptest-in-" ++ show ix ++ ".dat"
      out_file = "fptest-out-" ++ show ix ++ ".dat"
      cls_file = "fptest-" ++ show ix ++ ".cls"
  let enc :: Int -> [Word32] -> BS.ByteString -> IO Int
      enc n [] bs = do
        BS.writeFile in_file bs
        return n
      enc !n (w:ws) !bs = do
        enc (n+1) ws (bs`BS.append`toByteStringU32LE w)
  n <- enc 0 w32s BS.empty
  let kernel =
        "w32_to_f16_rt" ++ roundSuffix r
  writeFile cls_file $
    "let IN=file<bin>('" ++ in_file ++ "'):r\n" ++
    "let OUT=0:w\n" ++
    "#1`halfgen.cl`" ++ kernel ++ "<" ++ show n ++ ">(IN,OUT)\n" ++
    "save('" ++ out_file ++ "',OUT)\n"
  out_str <- readProcess "cls64" [cls_file] ""
  -- putStr out_str
  es <- toWords fromByteStringU16LE <$> BS.readFile out_file
  length es`seq`return ()
  removeFile in_file
  removeFile cls_file
  removeFile out_file
  return es
  -- putStrLn out_str
  -- let out = lines out_str
  -- case filter ("00000:"`isPrefixOf`) out of
  --   [ln] ->
  --     case words ln of
  --       [_,w16_str] ->
  --         case reads w16_str of
  --           [(w16,"")] -> return w16
  --           _ -> die "cannot find output (malformed owrd)"
  --       _ -> die "cannot find output (in line)"
  --   _ -> die "cannot find output"

roundSuffix :: Round -> String
roundSuffix = map toLower . drop (length "Round") . show



-- ensures that all f16's convert exactly to f32s
testRandF32s :: IO ()
testRandF32s = do
  testRandF32sR RoundZ
  testRandF32sR RoundE
  testRandF32sR RoundN
  testRandF32sR RoundP

testRandF32sR :: Round -> IO ()
testRandF32sR r = do
  putStrLn $ "testing F32->F16(" ++ show r ++")"
  bs_f32s <- BS.readFile "rand-f32s.bin"
  bs_f16s <- BS.readFile $ "rand-f32s-f16" ++ roundSuffix r ++ ".bin"
  let w16s = toWords fromByteStringU16LE bs_f16s
      w32s = toWords fromByteStringU32LE bs_f32s
  --
  let ws = zip w32s w16s
  mapM_ (uncurry (testFromF32 r)) ws
  return ()

testFromF32 :: Round -> Word32 -> Word16 -> IO Bool
testFromF32 r w32r w16r = do
  let f32r = floatFromBits w32r
      f16r = halfFromBits w16r
      f16x = floatToHalfRound r f32r
  let equal = halfIsNaN f16x && halfIsNaN f16r || f16x == f16r
  unless equal $ do
    putStrLn $ "under " ++ show r
    putStrLn $ "F32:   " ++ fmtF32 f32r
    putStrLn $ "  REF: " ++ fmtF16 f16r
    putStrLn $ "  ERR: " ++ fmtF16 f16x
  return equal
--    die "stop"


-- ensures that all f16's convert exactly to f32s
testToF32s :: IO ()
testToF32s = do
  bs_f16 <- BS.readFile "f16s.bin"
  bs_f32 <- BS.readFile "f16s-to-f32.bin"
  let w16s = toWords fromByteStringU16LE bs_f16
      w32s = toWords fromByteStringU32LE bs_f32
  --
  let ws = zip w16s w32s
  mapM_ (uncurry testToF32) ws
  return ()
--  mapM_ print
--  mapM_ (testH2F_ False True) (map halfFromBits w16s)

testToF32 :: Word16 -> Word32 -> IO ()
testToF32 w16 w32 = do
  let f16a = halfFromBits w16
      f32 = floatFromBits w32
      f16b = floatToHalf f32
  let equal = halfIsNaN f16a && halfIsNaN f16b || f16a == f16b
  when (not equal) $ do
    putStrLn $ "  " ++ fmtF16 f16a
    putStrLn "converted on HW to f32"
    putStrLn $ "  " ++ fmtF32 f32
    putStrLn "and converts back to f16"
    putStrLn $ "  " ++ fmtF16 f16b
    putStrLn ""
    die "stop"


extractF32 :: Float -> IO ()
extractF32 f32 = do
    let w32 = floatToBits f32
        (e,m) = ((f32_exp_mask .&. w32)`shiftR`23, w32 .&. f32_mnt_mask)
    putStrLn $ printf "exp: %d; mnt:x%X" e m


fmtF16 :: Half -> String
fmtF16 h = bin_w16 ++ printf " 0x%04X    " (halfToBits h) ++ "  " ++ show h
  where bin_w16 =
          replicate 3 ' ' ++
          fmtBinaryW 1 s ++ "`" ++
          fmtBinaryW 5 e ++ "`" ++
          fmtBinaryW 10 m ++ replicate (16 - 3) ' '
        w = halfToBits h
        s = ((f16_sign_bit .&. w)`shiftR`15)
        e = ((f16_exp_mask .&. w)`shiftR`10)
        m = (f16_mnt_mask .&. w)
fmtF16x :: Word16 -> String
fmtF16x = fmtF16 . halfFromBits

fmtF32 :: Float -> String
fmtF32 f = bin_w32 ++ printf " 0x%08X"     (floatToBits f) ++ "  " ++ show f
  where bin_w32 =
          fmtBinaryW 1 s ++ "`" ++
          fmtBinaryW 8 e ++ "`" ++
          fmtBinaryW 23 m
        w = floatToBits f
        s = ((f32_sign_bit .&. w)`shiftR`31)
        e = ((f32_exp_mask .&. w)`shiftR`23)
        m = (f32_mnt_mask .&. w)
fmtF32x :: Word32 -> String
fmtF32x = fmtF32 . floatFromBits


testH2F :: Half -> IO ()
testH2F h = do
  putStrLn $ fmtF16 h
  putStrLn "  converts to ===>"
  let f32 = halfToFloat h
  putStrLn $ fmtF32 f32
  putStrLn "  converts back to ===>"
  putStrLn $ fmtF16 (floatToHalf f32)

-- class (Show f,FiniteBits b) => TestFloat f b | f -> b where
-- class (Show f,FiniteBits b) => TestFloat f b where
--   tfToBits :: f -> b
--   tfFromBits :: b -> f
--   tfBitSizes :: (Int,Int) -- exp and mantissa bits

-- cannot deduce TestFloat f b0 from context TestFloat f
--   classify :: Show f => f -> IO ()
--   classify f = do
--     let (es,ms) = tfBitSizes f
--         b = tfToBits f
--     putStr $ show f
--     return ()



-- instance TestFloat Double Word64 where
--   tfToBits = floatToBits
--   tfFromBits = floatFromBits
--   tfBitSizes _ = (11,52)
-- instance TestFloat Float Word32 where
--   tfToBits = floatToBits
--   tfFromBits = floatFromBits
--   tfBitSizes _ = (8,23)
-- instance TestFloat Half Word16 where
--   tfToBits = halfToBits
--   tfFromBits = halfFromBits
--   tfBitSizes _ = (5,10)



----------------------------------
-- testF2H ( 1.0f) -- 3C000000 ==> 3C00sf
-- testF2H (-2.0f) -- C0000000 ==> C000sf
testF2H :: Float -> IO ()
testF2H f = do
  putStrLn $ "F32: " ++ fmtF32 f
  putStrLn $ "F16: " ++ fmtF16 (floatToHalf f)


testFuncsEq :: (Show a, Show b, Eq c) => (a -> b -> c) -> (a -> b -> c) -> [a] -> [b] -> IO ()
testFuncsEq f1 f2 as bs = do
  let abs = [(a,b) | a<-as, b<-bs]
  forM_ abs $ \ab ->
    when (uncurry f1 ab /= uncurry f2 ab) $ do
      die $ "functions differ on inputs: " ++ show ab

-- f1 mnt_len bits = if mnt_len < bits then (bits - mnt_len) else 0
-- f2 mnt_len bits = max 0 (bits - mnt_len)