import NVT.RawInst

import NVT.Bits
import NVT.IR
import NVT.Diagnostic
import NVT.Encoders.Codable
import NVT.Encoders.InstEncoder
import NVT.Parsers.InstParser

import qualified System.Console.ANSI as SCA -- cabal install ansi-terminal

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Word
import Debug.Trace
import System.IO
import Text.Printf

t = testInst "000FE20000000F00`C200000000247802:  MOV R36, 0xc2000000 {!1} ; // examples/sm_75/samples\bicubicTexture_cuda.sass:2235"
s0 = "/*0000*/       MOV              R1, c[0x0][0x28] {!8,Y};                   /* 000FD00000000F00`00000A0000017A02 */"
s1 = "/*0000*/       IMAD.MOV.U32     R1, RZ, RZ, c[0x0][0x28] {!8,Y};           /* 000FD000078E00FF`00000A00FF017624 */"
ia3 = "000FC80007F1E0FF`00005A0000027A10:        IADD3 R2, P0, R0, c[0x0][0x168], RZ {!4,Y} ; // examples/sm_75/samples\alignedTypes.sass:3122"

testInst :: String -> IO ()
testInst syntax =
  case parseSampleInst syntax of
    Left err ->
      putStrLn $ show 1 ++ ". " ++ err ++ ": malformed sample instruction\n"
    Right si -> do
      let syntax_x = stripPrefixBits syntax
      testSampleInst True si ("<interactive>",1) syntax_x
      return ()

testIADD3 = testFile "examples\\sm_75\\ops\\IADD3.sass"
testS2R = testFile "examples\\sm_75\\ops\\S2R.sass"
testMOV = testFile "examples\\sm_75\\ops\\MOV.sass"

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


stripPrefixBits :: String -> String
stripPrefixBits ln =
  case span (/=':') ln of
    (pfx,':':sfx) ->
      case span (/='`') pfx of
        (hi_str,'`':lo_str)
          | length hi_str == 16 && length lo_str == 16 && all isHexDigit (lo_str++hi_str) -> sfx
          | otherwise -> ln
        _ -> ln
    _ -> ln


testFile :: FilePath -> IO Bool
testFile fp = do
  let testLoop [] = return True
      testLoop ((lno,ln):lns)
        | instHasLabels ln = do
          -- should be able to parse it
          let syntax = stripPrefixBits ln
          case parseUnresolvedInst 0 fp 1 syntax of
            Left err -> do
              -- putStrLn $ "==> " ++ show ln
              let fmtDiag = dFormatWithLines (lines syntax)
              putStrLn "ERROR: parsed SampleInst, but InstParser failed"
              putStrLn $ fmtDiag err
              return False
            Right _ -> do
              --
              -- putStrLn $ "SKIPPING LABEL CASE: " ++ ln
              testLoop lns
        | otherwise = do
          -- 0001E80000100000`0000042908007387:        STL.U8 [R8+0x4], R41 {!4,+1.R} ;       // examples/sm_75/samples\cdpSimplePrint.sass:1519
          case parseSampleInst ln of
            Left err -> do
              putStrLn $ "SKIPPING: " ++ ln
              testLoop lns
            Right si -> do
              putStrLn ln
              z <- testSampleInst False si (fp,lno) (stripPrefixBits ln)
              if not z then return False
                else do
                  testLoop lns
  flns <- lines <$> readFile fp
  testLoop $ zip [1..] flns

-- 000FE40000000F00`0000000000147802:        MOV R20, 32@lo((_Z21computeBezierLinesCDPP10BezierLinei + .L_4@srel)) {!2} ; // examples/sm_75/samples\BezierLineCDP.sass:1526
-- 000FD00000000F00`0000000000157802:        MOV R21, 32@hi((_Z21computeBezierLinesCDPP10BezierLinei + .L_4@srel)) {!8,Y} ; // examples/sm_75/samples\BezierLineCDP.sass:1527
-- 000FC40000000F00`0000000000257802:        MOV R37, `(smem) {!2,Y} ;              // examples/sm_75/samples\cdpQuadtree.sass:6842
-- 000FD00000000F00`0000000000037802:        MOV R3, `($___ZZ10cdp_kerneliiiiE5s_uid__349) {!8,Y} ; // examples/sm_75/samples\cdpSimplePrint.sass:1663
instHasLabels :: String -> Bool
instHasLabels ln = any (`isInfixOf`ln) ["32@lo(","32@hi(","`(","@srel"]

testSampleInst :: Bool -> SampleInst -> (FilePath,Int) -> String -> IO Bool
testSampleInst verbose si (fp,lno) syntax = do
  let fmtDiag = dFormatWithLines (lines syntax)
  case parseInst 0 fp lno syntax of
    Left err -> do
      putStrLn "ERROR: parsed SampleInst, but InstParser failed"
      putStrLn $ fmtDiag err
      return False
    Right (i,ws) -> do
      mapM_ (putStrLn . fmtDiag) ws
      let i_formatted = format i
      case parseInst 0 fp lno i_formatted of
        Left err -> do
          putStrLn $ fp ++ ":"++ show lno ++ ": " ++ syntax
          putStrLn $ "formatted as ==> " ++ i_formatted
          putStrLn $ "but failed to re-parse"
          putStrLn $ fmtDiag err
          return False
        Right (i2,_)
          | i /= i2 -> do
            putStrLn $ fp ++ ":"++ show lno ++ ": " ++ syntax
            putStrLn $ "formatted as ==> " ++ i_formatted
            putStrLn $ "re-parsed differently"
            putStrLn $ fmtInstIr i
            putStrLn "========"
            putStrLn $ fmtInstIr i2
            return False
          | otherwise -> do
            case runInstDbgEncoder i of
              Left err -> do
                putStrLn $ "encode failed: " ++ fmtDiag err
                return False
              Right (w_enc,ws_enc,fs_enc) -> do
                mapM_ (putStrLn . fmtDiag) ws_enc
                oks_diags <-
                  forM (insertReservedFields fs_enc) $ \(f,f_val) -> do
                    let ref = getField128 (fOffset f) (fLength f) (siBits si)
                    let encd = getField128 (fOffset f) (fLength f) w_enc
                    let diag = do
                          let putStrFunc
                                | encd /= ref = putStrRed
                                | otherwise = putStr
                          if encd /= f_val then putStr " ==> " else putStr "     "
                          putStrFunc $
                            printf "    %-24s   %49s  %49s\n"
                              (fmtField f)
                              (fHexFieldWithMeaning f (siBits si))
                              (fHexFieldWithMeaning f w_enc)
                    return (ref==encd,diag)
                let (oks,diags) = unzip oks_diags
                    all_ok = and oks
                unless all_ok $ do
                  putStrLn "encoding mismatch"
                  putStrLn ""
                when (not all_ok || verbose) $ do
                  putStrLn syntax
                  putStrLn ""
                  putStrLn $ fmtInstIr i
                  putStrLn ""
                  putStrLn "=== encoded fields ==="
                  putStrLn $ "     " ++ printf "    %-24s   %16s %32s  %16s %32s" "FIELD" "REFERENCE" "" "ENCODED" ""
                  sequence diags
                  putStrLn ""
                  putStrLn "== bit by bit ==="
                  compareBits (siBits si) w_enc
                return all_ok

insertReservedFields :: [(Field,Word64)] -> [(Field,Word64)]
insertReservedFields = loop 0
  where loop :: Int -> [(Field,Word64)] -> [(Field,Word64)]
        loop off []
          | off < 64 = (fReserved off (64-off),0) : loop 64 []
          | off < 128 = [(fReserved off (128-off),0)]
          | otherwise = []
        loop off ((f,v):fvs)
          | fOffset f == off = (f,v) : loop (off+fLength f) fvs
          | off < 64 && fOffset f >= 64 = fv_to_64 : loop 64 ((f,v):fvs)
          | otherwise =  fv_to_next : loop (fOffset f) ((f,v):fvs)
          where fv_to_64 = (fReserved off (64-off),0)
                fv_to_next = (fReserved off (fOffset f-off),0)



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

fHexFieldWithMeaning :: Field -> Word128 -> String
fHexFieldWithMeaning f w128 =
    printf "%16s %-32s" (fHexField f val) ("(" ++ fFormat f w128 val ++ ")")
  where val = getField128 (fOffset f) (fLength f) w128

fHexField :: Field -> Word64 -> String
fHexField f =
  printf ("0x%0" ++ show ((fLength f + 3)`div`4) ++ "X")

fmtField :: Field -> String
fmtField f = fName f ++ fmtFieldIndices (fOffset f,fLength f)
fmtFieldIndices :: (Int,Int) -> String
fmtFieldIndices (off,len) = "[" ++ body ++ "]"
  where body
          | len == 1 = show off
          | otherwise = show (off + len - 1) ++ ":" ++ show off





















putStrGreen :: String -> IO ()
putStrGreen = hPutStrColored (sgr SCA.Vivid SCA.Green) stdout
putStrRed :: String -> IO ()
putStrRed = hPutStrColored (sgr SCA.Vivid SCA.Red) stdout
putStrYellow :: String -> IO ()
putStrYellow = hPutStrColored (sgr SCA.Vivid SCA.Yellow) stdout
putStrWhite :: String -> IO ()
putStrWhite = hPutStrColored (sgr SCA.Vivid SCA.White) stdout

sgr i c = SCA.SetColor SCA.Foreground i c

hPutStrColored :: SCA.SGR -> Handle -> String -> IO ()
hPutStrColored sgr h str = bracket_ acq rel act
  where acq = SCA.hSetSGR h [sgr]
        act = hPutStr h str
        rel = SCA.hSetSGR h [SCA.Reset]


-- compareEncoding :: Word128 -> Word128
-- compareEncoding =

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