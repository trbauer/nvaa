import NVT.RawInst

import NVT.Bits
import NVT.Diagnostic
import NVT.Encoders.InstEncoder
-- import NVT.Encoders.RawInstEncoderSM75
import NVT.Parsers.InstParser

import Control.Applicative
import Control.Monad
import Data.Word
import Text.Printf



main :: IO ()
main = do
  flns <- drop 1 . lines <$> readFile "bs-raw.sass"
  putStrLn $ parseLines flns
  writeFile "test-file.sass" $ parseLines flns

parseLines :: [String] -> String
parseLines [] = ""
parseLines (ln0:ln1:lns) =
  case parseSampleInst (ln0 ++ "\n" ++ ln1) of
    Left _ -> parseLines (ln1:lns)
    Right si ->
      fmtSampleInst si ++ "\n" ++ parseLines lns

t = "/*0000*/       MOV              R1, c[0x0][0x28] {!8,Y};                   /* 000FD00000000F00`00000A0000017A02 */"
--  /*0000*/       MOV              R1, c[0x0][0x28] {!8,Y};                   /* 000FD00000000F00`00000A0000017A02 */
testInst :: String -> IO ()
testInst s =
  case parseSampleInst s of
    Left err -> putStrLn $ err ++ ": malformed sample instruction"
    Right si -> do
      case parseInst 0 "<interactive>" s of
        Left err -> putStrLn $ dFormatWithLines (lines s) err
        Right (i,ws) -> do
          mapM_ (putStrLn . dFormatWithLines (lines s)) ws
          print i
      return ()


{-
      case encodeRawInstDebug (siRawInst si) of
        Left err -> putStrLn $ dFormat err
        Right (bits,ws,fs) -> do
          mapM_ (putStrLn . dFormat) ws
          let emitField :: (Field,Word64) -> IO ()
              emitField (f,val) = do
                let val_sut = getField128 (fOffset f) (fLength f) bits
                    val_ref = getField128 (fOffset f) (fLength f) (siBits si)
                when (val_sut /= val_ref) $ do
                  let patt = "%-24s  " ++ fHexField f ++ " " ++ fHexField f
                  putStrLn $ printf patt (fmtField f ++ ":") val_sut val_ref
          mapM_ emitField fs
          compareBits (siBits si) bits
      return ()
-}

compareBits :: Word128 -> Word128 -> IO ()
compareBits w_ref w_sut = cmpBits [0 .. 127]
  where cmpBits :: [Int] -> IO ()
        cmpBits [] = return ()
        cmpBits (ix:ixs) = do
          if not (ixDiffers ix) then cmpBits ixs
            else do
              let (nxt,ixs_sfx) = span ixDiffers ixs
                  len = if null nxt then 1 else last nxt - ix + 1
                  val_sut = getField128 ix 1 w_sut
                  val_ref = getField128 ix 1 w_ref
              putStrLn $
                "Bits" ++
                  printf "%-24s" (fmtFieldIndices (ix,len)++":") ++
                  printf " 0x%0X     vs.    0x%0X" val_ref val_sut
              cmpBits ixs_sfx

        ixDiffers ix = getField128 ix 1 w_ref /= getField128 ix 1 w_sut

fHexField :: Field -> String
fHexField f =
  "0x%0" ++ show ((fLength f + 3)`div`4) ++ "X"

fmtField :: Field -> String
fmtField f = fName f ++ fmtFieldIndices (fOffset f,fLength f)
fmtFieldIndices :: (Int,Int) -> String
fmtFieldIndices (off,len) = "[" ++ body ++ "]"
  where body
          | len == 1 = show off
          | otherwise = show (off + len - 1) ++ ":" ++ show off