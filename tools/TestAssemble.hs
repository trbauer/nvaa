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
import Data.List
import Data.Word
import Debug.Trace
import System.IO
import Text.Printf


movIt = testFile "examples\\sm_75\\ops\\MOV.sass"

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

s0 = "/*0000*/       MOV              R1, c[0x0][0x28] {!8,Y};                   /* 000FD00000000F00`00000A0000017A02 */"
s1 = "/*0000*/       IMAD.MOV.U32     R1, RZ, RZ, c[0x0][0x28] {!8,Y};           /* 000FD000078E00FF`00000A00FF017624 */"


testFile :: FilePath -> IO Bool
testFile fp = do
  let testLoop [] = return True
      testLoop ((lno,ln):lns)
        | any (`isInfixOf`ln) ["32@lo(","32@hi("] = do
          putStrLn $ "SKIPPING LABEL CASE: " ++ ln
          testLoop lns
        | otherwise = do
          -- 0001E80000100000`0000042908007387:        STL.U8 [R8+0x4], R41 {!4,+1.R} ;       // examples/sm_75/samples\cdpSimplePrint.sass:1519
          case parseSampleInst ln of
            Left err -> do
              putStrLn $ "SKIPPING: " ++ ln
              testLoop lns
            Right si -> do
              z <- testSampleInst False si (fp,lno) (fmtRawInst (siRawInst si)) -- ln
              if not z then return False
                else do
                  putStrLn ln
                  testLoop lns
  flns <- lines <$> readFile fp
  testLoop $ zip [1..] flns


testInst :: Int -> String -> IO ()
testInst lno syntax =
  case parseSampleInst syntax of
    Left err ->
      putStrLn $ show lno ++ ". " ++ err ++ ": malformed sample instruction\n"
    Right si -> do
      testSampleInst True si ("<interactive>",1) syntax
      return ()

testSampleInst :: Bool -> SampleInst -> (FilePath,Int) -> String -> IO Bool
testSampleInst verbose si (fp,lno) syntax = do
  let fmtDiag = dFormatWithLines (lines syntax)
  case parseInst lno fp syntax of
    Left err -> do
      putStrLn "ERROR: parsed SampleInst, but InstParser failed"
      putStrLn $ fmtDiag err
      return False
    Right (i,lbls,ws) -> do
      unless (null lbls) $
        fail "TestAssemble: labels not supported yet"
      mapM_ (putStrLn . fmtDiag) ws
      let i_formatted = format i
      case parseInst lno fp i_formatted of
        Left err -> do
          putStrLn $ fp ++ ":"++ show lno ++ ": " ++ syntax
          putStrLn $ "formatted as ==> " ++ i_formatted
          putStrLn $ "but failed to re-parse"
          putStrLn $ fmtDiag err
          return False
        Right (i2,_,_)
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
                            printf "    %-24s   %24s  %24s\n"
                            (fmtField f) (fHexField f ref) (fHexField f encd)
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
                  putStrLn $ "     " ++ printf "    %-24s   %24s  %24s" "FIELD" "REFERENCE" "ENCODED"
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