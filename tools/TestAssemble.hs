import NVT.RawInst

import NVT.Bits
import NVT.IR
import NVT.Loc
import NVT.Diagnostic
import NVT.Encoders.Codable
import NVT.Encoders.InstEncoder
import NVT.Parsers.InstParser

import qualified System.Console.ANSI as SCA -- cabal install ansi-terminal

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Word
import Debug.Trace
import System.Directory
import System.IO
import Text.Printf

-- currently:
--   * first SI test passes!!!!!!!!!!!!!!!!!

--   => make nvaa handle .exe and .dll files
{-
        code for sm_75
                Function : micro
        .headerflags    @"EF_CUDA_SM75 EF_CUDA_PTX_SM(EF_CUDA_SM75)"
        /*0000*/                   IMAD.MOV.U32 R1, RZ, RZ, c[0x0][0x28] ;  /* 0x00000a00ff017624 */
                                                                            /* 0x000fd000078e00ff */
        /*0010*/                   IMAD.MOV.U32 R1, RZ, RZ, c[0x0][0x28] ;  /* 0x00000a00ff017624 */
                                                                            /* 0x000fd000078e00ff */
        /*0020*/                   S2R R0, SR_CTAID.X ;                     /* 0x0000000000007919 */
                                                                            /* 0x000e220000002500 */
        /*0030*/                   IMAD.MOV.U32 R37, RZ, RZ, 0x4 ;          /* 0x00000004ff257424 */
                                                                            /* 0x000fc600078e00ff */
        /*0040*/                   S2R R3, SR_TID.X ;                       /* 0x0000000000037919 */
                                                                            /* 0x000e240000002100 */
        /*0050*/                   IMAD R0, R0, c[0x0][0x0], R3 ;           /* 0x0000000000007a24 */
                                                                            /* 0x001fc800078e0203 */
        /*0060*/                   IMAD.WIDE R2, R0, R37, c[0x0][0x160] ;   /* 0x0000580000027625 */
                                                                            /* 0x000fd400078e0225 */
        /*0070*/                   MOV R35, 0x24 ;                          /* 0x0000002400237802 */
                                                                            /* 0x000fd40000000f00 */
        /*0080*/                   STG.E.SYS [R2], R35 ;                    /* 0x0000002302007386 */
                                                                            /* 0x000fe2000010e900 */
        /*0090*/                   EXIT ;                                   /* 0x000000000000794d */
                                                                            /* 0x000fea0003800000 */
        /*00a0*/                   BRA 0xa0;                                /* 0xfffffff000007947 */
                                                                            /* 0x000fc0000383ffff */
-}
-- [11:9]  [90] [91]  [31:24]
--    1     *    0     /=RZ       LDG.E.64.SYS R16, [R24] {!1,+4.W} ;
--    4     0    1     /=RZ       LDG.E.64.SYS R16, [R254.U32+UR0]
--    4     0    1     ==RZ       LDG.E.64.SYS R16, [UR0]  // probably [RZ.U32+UR0]
--    4     1    1     /=RZ       LDG.E.64.SYS R16, [R254.64+UR0]
--    4     1    1     ==RZ       LDG.E.64.SYS R16, [UR0]  // probably [RZ.U32+UR0]


-- 000EE200001EEB00`0000000018107381:        LDG.E.64.SYS R16, [R24] {!1,+4.W} ;
-- 000EE2000C1EEB00`00000004FF107981:        LDG.E.64.SYS R16, [UR4] {!1,+4.W} ;

--

testIADD3 = testFile "examples\\sm_75\\ops\\IADD3.sass"
testIMAD = testFile "examples\\sm_75\\ops\\IMAD.sass"
testS2R = testFile "examples\\sm_75\\ops\\S2R.sass"
testMOV = testFile "examples\\sm_75\\ops\\MOV.sass"
testISETP = testFile "examples\\sm_75\\ops\\ISETP.sass"
--
testLD = testFile "examples\\sm_75\\ops\\LD.sass"
testLDG = testFile "examples\\sm_75\\ops\\LDG.sass"
testLDL = testFile "examples\\sm_75\\ops\\LDL.sass"
testLDS = testFile "examples\\sm_75\\ops\\LDS.sass"
--
testST = testFile "examples\\sm_75\\ops\\ST.sass"
testSTG = testFile "examples\\sm_75\\ops\\STG.sass"
testSTL = testFile "examples\\sm_75\\ops\\STL.sass"
testSTS = testFile "examples\\sm_75\\ops\\STS.sass"
-- 0041E8000010ED00`0000100402007386:        STG.E.128.SYS [R2+0x10], R4 {!4,+1.R,^3}

-- 000FD80003F04070`727FFFFF0000780C
-- 000F`D800`03F0`4070
--        96|  80|
-- t = testInst "000FD80003F04070`727FFFFF0000780C:        ISETP.GT.U32.AND P0, PT, R0, 0x727fffff, PT {!12,Y} ;"
-- t = testInst "000FC00000000000`0000000000007918:        NOP  {Y} ;"
t = testInst "0003E8000C10E904`0000000FFF007986:        STG.E.SYS [UR4], R15 {!4,+2.R} ;"

tld = testInst "000EE800001EE900`FFFFFC0012127381:        LDG.E.SYS R18, [R18+-0x4] {!4,+4.W} ;"



s0 = "/*0000*/       MOV              R1, c[0x0][0x28] {!8,Y};                   /* 000FD00000000F00`00000A0000017A02 */"
s1 = "/*0000*/       IMAD.MOV.U32     R1, RZ, RZ, c[0x0][0x28] {!8,Y};           /* 000FD000078E00FF`00000A00FF017624 */"
ia3 = "000FC80007F1E0FF`00005A0000027A10:        IADD3 R2, P0, R0, c[0x0][0x168], RZ {!4,Y} ; // examples/sm_75/samples\alignedTypes.sass:3122"
---         IADD3 R2, P0, R0, c[0x0][0x168], RZ {!4,Y} ;
-- |........|........|........|........|........|........
-- 1        11       21       31       41       51
imad_mov_u32 = "000FD000078E00FF`00000A00FF017624: IMAD.MOV.U32     R1, RZ, RZ, c[0x0][0x28] {!8,Y};"


imad = testInst "000FE200078E0A06`0000000105067824:        IMAD.IADD R6, R5, 0x1, -R6 {!1};"

testB =
  testBlocks $
    "IMAD.MOV.U32     R1, RZ, RZ, c[0x0][0x28] {!8,Y};\n" ++
    "S2R              R0, SR_CTAID.X {!1,+1.W};\n"++
    ""


help :: IO ()
help = putStrLn $
  "testAllFF      - runs all tests failing fast\n"++
  "testInst       - once a test fails cut and paste the sample into testInst to isolate\n"++
  ""

testAll, testAllFF :: IO Bool
testAll = testAllFF
testAllFF = testAllG True
testAllNFF :: IO Bool
testAllNFF = testAllG False

-- my minimal program to write effects out is:
--   IMAD.MOV.U32     R1, RZ, RZ, c[0x0][0x28] {!8,Y};      000FD000078E00FF`00000A00FF017624
--   S2R              R0, SR_CTAID.X {!1,+1.W};
--   IMAD.MOV.U32     R37, RZ, RZ, 0x4 {!3,Y};              000FC600078E00FF`00000004FF257424
--   S2R              R3, SR_TID.X {!2,+1.W};
--   IMAD             R0, R0, c[0x0][0x0], R3 {!4,Y,^1}     001FC800078E0203`0000000000007A24
--   IMAD.WIDE        R2, R0, R37, c[0x0][0x160] {!10,Y};   000FD400078E0225`0000580000027625
--
-- TO READ:
--   LDG.E.SYS        R4, [R2] {!4,+3.W,+1.R}
--
-- TO WRITE:
--   STG.E.SYS        [R2], R35 {!1};
testInst :: String -> IO ()
testInst syntax =
  case parseSampleInst syntax of
    Left err ->
      putStrLn $ show 1 ++ ". " ++ err ++ ": malformed sample instruction\n"
    Right si -> do
      let syntax_x = stripPrefixBits syntax
      testSampleInst id True si ("<interactive>",1) syntax_x
      return ()

testParseInst :: String -> IO ()
testParseInst syntax = do
  let fmtDiag = dFormatWithLines (lines syntax)
  case parseInst 0 "" 1 syntax of
    Left err -> do
      putStrLn "ERROR: parsed SampleInst, but InstParser failed"
      putStrLn $ fmtDiag err
    Right (i,ws) -> do
      mapM_ (putStrLn . fmtDiag) ws
      putStrLn $ fmtInstIr i



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



testAllG :: Bool -> IO Bool
testAllG fail_fast = do
  putStrLn $ replicate 70 '*'
  let runTestFiles rs [] = return (reverse rs)
      runTestFiles rs (sass_f:sass_fs) = do
        (np,nt) <- testOneInstFile ("tests/"++sass_f)
        if fail_fast && np /= nt then return ((np,nt):rs)
          else runTestFiles ((np,nt):rs) sass_fs
  --
  sass_fs <- filter (".sass"`isSuffixOf`) <$> listDirectory "tests"
  (nps,nts) <- unzip <$> runTestFiles [] sass_fs
  let (np,nt) = (sum nps,sum nts)
  putStrLn $ "******** passed " ++ show np ++ " of " ++ show nt ++ " ********"
  return $ np == nt

testMnemonic :: String -> IO (Int,Int)
testMnemonic mne = testOneInstFile ("tests/" ++ mne ++ ".sass")
testOneInstFile :: FilePath -> IO (Int,Int)
testOneInstFile sass_f = do
  putStr $ printf "  %-32s" (sass_f++":") ++ "  "
  -- putStrLn $ "************ TESTING " ++ sass_f ++ " ************"
  ior <- newIORef ([] :: [IO ()])
  let recordIO :: IO () -> IO ()
      recordIO act = modifyIORef ior ((act:))
  (np,nt) <- testFileG recordIO sass_f
  if np == nt then putStrGreen "SUCCESS" >> putStrLn ("  (" ++ show nt ++ " tests)")
    else do
      putStrRed "FAILED\n"
      racts <- readIORef ior
      sequence_ (reverse racts)
  return (np,nt)

testFile :: FilePath -> IO (Int,Int)
testFile = testFileGK id (-1)
testFileG :: (IO () -> IO ()) -> FilePath -> IO (Int,Int)
testFileG emit_io = testFileGK emit_io (-1)
testFileGK ::  (IO () -> IO ()) -> Int -> FilePath -> IO (Int,Int)
testFileGK emit_io k fp = do
  flns <- lines <$> readFile fp
  let testLoop (np,nt) [] = return (np,nt)
      testLoop (np,nt) ((lno,ln):lns)
        | null skipped_spaces || "//"`isPrefixOf`skipped_spaces = testLoop (np,nt) lns
        | instHasLabels ln = do
          -- should be able to parse it
          let syntax = stripPrefixBits ln
          case parseUnresolvedInst 0 fp 1 syntax of
            Left err -> do
              -- putStrLn $ "==> " ++ show ln
              let fmtDiag = dFormatWithLines flns
              emit_io $ putStrLn "ERROR: parsed SampleInst, but InstParser failed"
              emit_io $ putStrLn $ fmtDiag err
              return (np,nt+1)
            Right _ -> do
              --
              -- putStrLn $ "SKIPPING LABEL CASE: " ++ ln
              testLoop (np,nt) lns
        | otherwise = do
          -- 0001E80000100000`0000042908007387:        STL.U8 [R8+0x4], R41 {!4,+1.R} ;       // examples/sm_75/samples\cdpSimplePrint.sass:1519
          case parseSampleInst ln of
            Left err -> do
              emit_io $ putStrLn $ "FAILED TO PARSE SAMPLE: " ++ ln
              testLoop (np,nt+1) lns
            Right si -> do
              emit_io $ putStrYellow (ln++"\n")
              z <- testSampleInst emit_io False si (fp,lno) (stripPrefixBits ln)
              if not z then return (np,nt+1)
                else do
                  testLoop (np+1,nt+1) lns
        where skipped_spaces = dropWhile isSpace ln
  let takePrefix = if k >= 0 then take k else id
  testLoop (0,0) $ zip [1..] (takePrefix flns)

-- 000FE40000000F00`0000000000147802:        MOV R20, 32@lo((_Z21computeBezierLinesCDPP10BezierLinei + .L_4@srel)) {!2} ; // examples/sm_75/samples\BezierLineCDP.sass:1526
-- 000FD00000000F00`0000000000157802:        MOV R21, 32@hi((_Z21computeBezierLinesCDPP10BezierLinei + .L_4@srel)) {!8,Y} ; // examples/sm_75/samples\BezierLineCDP.sass:1527
-- 000FC40000000F00`0000000000257802:        MOV R37, `(smem) {!2,Y} ;              // examples/sm_75/samples\cdpQuadtree.sass:6842
-- 000FD00000000F00`0000000000037802:        MOV R3, `($___ZZ10cdp_kerneliiiiE5s_uid__349) {!8,Y} ; // examples/sm_75/samples\cdpSimplePrint.sass:1663
instHasLabels :: String -> Bool
instHasLabels ln = any (`isInfixOf`ln) ["32@lo(","32@hi(","`(","@srel"]

testSampleInst :: (IO () -> IO ()) -> Bool -> SampleInst -> (FilePath,Int) -> String -> IO Bool -- (Int,Int)
testSampleInst emit_io verbose si (fp,lno) syntax = do
  let fmtDiag d = dFormatWithLines syntax_lines d
        -- insert empty lines in "syntax" so the context can be shown
        -- (this function only gets a single line from a bigger file)
        -- this way we get both the right line number
        where syntax_lines = replicate (lno-1) "\n" ++ [syntax]
      emitLn = emit_io . putStrLn
      emitLnRed = emit_io . putStrRed
  case parseInst 0 fp lno syntax of
    Left err -> do
      emitLn "ERROR: parsed SampleInst, but InstParser failed"
      emitLn $ fmtDiag err
      return False
    Right (i,ws) -> do
      emit_io $ mapM_ (putStrLn . fmtDiag) ws
      let i_formatted = format i
          -- move the line number for "line 1" since we only get
          -- a line at a time
      case parseInst 0 fp lno i_formatted of
        Left err -> do
          emitLn $ fp ++ ":"++ show lno ++ ": " ++ syntax
          emitLn $ fmtInstIr i
          emitLn $ "formatted as ==> " ++ i_formatted
          emitLn $ "but failed to re-parse"
          emitLn $ dFormatWithLines (lines i_formatted) err
          return False
        Right (i2,_)
          | i{iLoc=lNONE} /= i2{iLoc=lNONE} -> do
            emitLn $ fp ++ ":"++ show lno ++ ": " ++ syntax
            emitLn $ "formatted as ==> " ++ i_formatted
            emitLn $ "re-parsed differently"
            emitLn $ fmtInstIr i
            emitLn "========"
            emitLn $ fmtInstIr i2
            return False
          | otherwise -> do
            case runInstDbgEncoder i of
              Left err -> do
                emitLn $ fmtInstIr i
                emitLnRed $ "encode failed: " ++ fmtDiag err ++ "\n"
                return False
              Right (w_enc,ws_enc,fs_enc) -> do
                emit_io $ mapM_ (putStrLn . fmtDiag) ws_enc
                oks_diags <-
                  forM (insertReservedFields fs_enc) $ \(f,f_val) -> do
                    let ref = getField128 (fOffset f) (fLength f) (siBits si)
                    let encd = getField128 (fOffset f) (fLength f) w_enc
                    let diag = do
                          let putStrFunc
                                | encd /= ref = putStrRed
                                | otherwise = putStr
                          if encd /= f_val then putStr " ==> " else putStr "     "
                          emit_io $ putStrFunc $
                            printf "    %-32s   %49s  %49s\n"
                              (fmtField f)
                              (fHexFieldWithMeaning f (siBits si))
                              (fHexFieldWithMeaning f w_enc)
                    return (ref==encd,diag)
                let (oks,diags) = unzip oks_diags
                    all_ok = and oks
                unless all_ok $ do
                  emit_io $ do
                    putStrLn "encoding mismatch"
                    putStrLn ""
                when (not all_ok || verbose) $ do
                  emitLn syntax
                  emitLn ""
                  emitLn $ fmtInstIr i
                  emitLn ""
                  emitLn "=== encoded fields ==="
                  emitLn $ "     " ++ printf "    %-32s   %16s %32s  %16s %32s" "FIELD" "REFERENCE" "" "ENCODED" ""
                  sequence diags
                  emitLn ""
                  when verbose $
                    emit_io $ if all_ok then putStrGreen "fields match\n" else putStrRed "fields mismatch\n"
                  emitLn "== bit by bit ==="
                  compareBits emit_io (siBits si) w_enc
                return all_ok

-- putStrSuccess :: Bool -> String -> IO ()
-- putStrSuccess z
--   | z = putStrGreen
--   | otherwise = putStrRed
-- putStrLnSuccess :: Bool -> String -> IO ()
-- putStrLnSuccess z s = putStrSuccess z (s++"\n")

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



compareBits :: (IO () -> IO ()) -> Word128 -> Word128 -> IO ()
compareBits emit_io w_ref w_sut = cmpBits [0 .. 127]
  where cmpBits :: [Int] -> IO ()
        cmpBits [] = return ()
        cmpBits (ix:ixs) = do
          if not (ixDiffers ix) then cmpBits ixs
            else do
              let (nxt,ixs_sfx) = span ixDiffers ixs
                  len = if null nxt then 1 else last nxt - ix + 1
                  val_sut = getField128 ix 1 w_sut
                  val_ref = getField128 ix 1 w_ref
              emit_io $ putStrLn $
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


