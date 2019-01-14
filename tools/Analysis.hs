module Analysis where

import NVT.Opts
import NVT.RawInst
import NVT.Word128
import NVT.CUDASDK

import Control.Exception
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Word
import Debug.Trace
import System.Directory
import System.FilePath
import System.IO.Unsafe
import System.Process
import Text.Printf
-- import qualified Data.Map.Strict as DM
import qualified Data.ByteString as S
import qualified Data.Map.Strict as DM

-- want to infer the bit patterns of all opcodes
-- for each opcode X
--   load all samples (with bits)
--   find the longest common field starting from 0 where all samples share the same value
--

stg :: IO Sample -- STG.E.SYS [R12], R17 {!2,+1.R} ;       // examples/sm_75/samples\boxFilter_kernel.sass:13563
stg = sString ("0001E4000010E900`000000110C007386")

-- LDG.E.SYS R3, [UR38+0x14] {!1,+5.W} ;  // examples/sm_75/samples\cdpQuadtree.sass:7890
ldg_ur_off = sString "000F22000C1EE900`00001426FF037981" --
-- LDG with UR
-- 000EA8000C1EE900`00000404FF047981:        LDG.E.SYS R4, [UR4+0x4] {!4,+3.W} ;    // examples/sm_75/samples\bisect_large.sass:5800
-- 000EE8000C1EE900`00000006FF057981:        LDG.E.SYS R5, [UR6] {!4,+4.W} ;        // examples/sm_75/samples\bisect_large.sass:5802
-- 000EA8000C1EE900`00000004FF027981:        LDG.E.SYS R2, [UR4] {!4,+3.W} ;        // examples/sm_75/samples\bitonic.sass:3258
-- 000EA8000C1EE900`00000404FF058981:  @!P0  LDG.E.SYS R5, [UR4+0x4] {!4,+3.W} ;    // examples/sm_75/samples\bitonic.sass:3260
-- 000EE8000C1EE900`00000006FF037981:        LDG.E.SYS R3, [UR6] {!4,+4.W} ;        // examples/sm_75/samples\bitonic.sass:3262
-- 000F22000C1EE900`00000406FF048981:  @!P0  LDG.E.SYS R4, [UR6+0x4] {!1,+5.W} ;    // examples/sm_75/samples\bitonic.sass:3264
--                          ^^ FF means UR

--                                           ^^
-- LDG.E.SYS R0, [R24+0x20] {!1,+3.W} ;   // examples/sm_75/samples\BezierLineCDP.sass:1600
ldg_v_off  = sString "000EA200001EE900`0000200018007381"
-- 000F6200001EE900`0000000004077381:        LDG.E.SYS R7, [R4] {!1,+6.W} ;         // examples/sm_75/samples\alignedTypes.sass:2896
-- 000EA8000C1EE900`00000004FF027981:        LDG.E.SYS R2, [UR4] {!4,+3.W} ;        // examples/sm_75/samples\bisect_large.sass:5798
--                  ........


showLoadControls :: IO ()
showLoadControls =
  twiddleField (75,2) (sString "000EA200001EE900`0000000004067381")

findLdPredLowBit = do
  -- 87 seemed to twidding things
  forM_ [87..100] $ \pix -> twiddleFieldsB [(82,2),(pix,1)]
    (sString "000EA200001EE900`0000000004067381")


-- FIXME:
--   start bit is the top part of another field, will this cause confusion
probeReserved :: Int -> Sample -> IO ()
probeReserved ix s = startProbe
  where startProbe :: IO ()
        startProbe = do
          [str0,str1] <- disassembleField (ix,1) s
          if str0 == str1 then extendFieldMbz 2
            else putStrLn "field has syntactic meaning"
        extendFieldMbz :: Int -> IO ()
        extendFieldMbz len = do
          putStrLn $ "extend field MBZ: " ++ show len
          strs <- disassembleField (ix,len) s
          if all (==head strs) strs then extendFieldMbz (len+1)
            else putStrLn $ "===> " ++ fFormat (ix,len-1) ++ ": is probably reserved <==="

probe :: Int -> Sample -> IO ()
probe ix s = startProbe
  where startProbe :: IO ()
        startProbe = do
          [str0,str1] <- disassembleField (ix,1) s
          if str0 == str1 then extendFieldMbz 2
            else extendField 2

        extendFieldMbz :: Int -> IO ()
        extendFieldMbz len = do
          putStrLn $ "extend field MBZ: " ++ show len
          strs <- disassembleField (ix,len) s
          if all (==head strs) strs then extendFieldMbz (len+1)
            else putStrLn $ "===> " ++ fFormat (ix,len-1) ++ ": is probably reserved <==="

        extendField :: Int -> IO ()
        extendField len = do
          putStrLn $ "extend field: " ++ show len
          strs <- disassembleField (ix,len) s
          let (fst_half,snd_half) = splitAt (len`div`2) strs
          if ix + len == 64 then do
              mapM print fst_half
              putStrLn $ "===> " ++ fFormat (ix,len-1) ++ ": is a field (stopping at bit 64) <==="
            else if ix + len == 128 then do
                mapM print fst_half
                putStrLn $ "===> " ++ fFormat (ix,len-1) ++ ": is a field (stopping at bit 128) <==="
              else if fst_half == snd_half then do
                  -- BUG: this only works if the field is up against an MBZ field...
                  mapM print fst_half
                  putStrLn $ "===> " ++ fFormat (ix,len-1) ++ ": is a field <==="
                else extendField (len + 1)




disassembleFieldD :: Field -> Sample -> IO [String]
disassembleFieldD f (Sample w128) = disBitsRawBatch opts{oVerbosity = 2} w128s
  where w128s :: [Word128]
        w128s = map (\v -> putField128 (fOff f) (fLen f) v w128) [0 .. 2^(fLen f)-1]
disassembleField :: Field -> Sample -> IO [String]
disassembleField f (Sample w128) = disBitsRawBatch opts w128s
  where w128s :: [Word128]
        w128s = map (\v -> putField128 (fOff f) (fLen f) v w128) [0 .. 2^(fLen f)-1]
-- disassembleFields :: [Field] -> Sample -> IO [String]
-- disassembleFields f (Sample w128) = disBitsRawBatch opts w128s
--  where w128s :: [Word128]
--        w128s = map (\v -> putField128 (fOff f) (fLen f) v w128) [0 .. 2^(fLen f)-1]


-- LDG
-- [14:12] predicat reg {.P0,.P1,.P2,.P3,.P4,.P5,.P6,.PT} -- PT doesn't show if [15] is not clear (since it's implied)
-- [15] predicate negation
-- [23:16] dst
-- [31:24] src0
-- [39:32] MBZ

-- [76] .PRIVATE
-- [78:77] = {.CTA,.SM,.GPU,.SYS}
-- [81:79] = {.CONSTANT,.SYS,.STRONG,.MMIO}
-- extensions are printed out in reverse order
--   LDG.E.CONSTANT.SYS.PRIVATE R6, [R4]
-- [82:83] = {P1,P3,P5,??}  enables predicate
--          looks like PredReg[2:1] PreReg[0] must be elsewhere or implicit
-- [84:]
-- [86:85] = {.EF,***,.EL,.LU,.EU,.NA,.INVALID6,.INVALID7}
--   *** means no syntax added (default?)
--          0 =>  LDG.E.SYS P5, R6, [R4]
--
-- ****************
-- [105]: Opclass 'ldg__sImmOffset', undefined value 0x10 for table 'TABLES_opex_0' at address 0x00000000

-- QUERIES:
--
--  * SAMPLE FIELDS
--    given fields Fs, list the values in a list of samples Ss
--             [15:10]    [27]
--     SYN1    0x4        ...
--     SYN2    0x4        ...
--     SYN3    0x3        ...
--     SYN4    0x4        ...
--     SYN5    0x4        ...
--
--
--   * TWIDDLE VALUES
--   Given a base instruction word W, enumerate syntax as we try all
--   values for fields Fs
--    [15:10]       [27]
--    0b000000 (0)    0     SYNTAX
--    0b000000 (0)    1     *** CRASH *** one-line msg
--    0b000001 (1)    0     SYNTAX
--    ...
--
-- Group ops into formats
-- Favor larger sets since they are more frequent (important)


-- samplesFromOps :: String -> IO [Sample]
-- samplesFromOps op_name = do
  -- cache ops?
  -- pick a subset
--  ...
help :: IO ()
help = do
  putStr $
    "COMMANDS\n" ++
    "  * listDiffs          => lists the different fields between two samples\n" ++
    "  * decode             => emits the syntax for a sample\n" ++
    "  * twiddleField[s]    => given fields, test all values and emit syntax\n" ++
    "  * twiddleFields[s]B  => same as above, but emit fields in binary\n" ++
    "  * findValues         => finds longest fields that match each input in the sequence of samples\n" ++
    "\n" ++
    "SAMPLE CONSTRUCTORS:\n" ++
    "  sString           => loads a bit pattern from a `-separated Word128 string\n" ++
    "  sFile             => loads samples from a file (sFile \"foo.sass\")\n" ++
    "  sAllOps[K]        => loads all ops with a given name (sAllOps \"LDC\")\n" ++
    ""

-- POSSIBLE ADDITIONS
-- "  * find fields cycle [better name]: twiddles a field, extending the length iteratively until the syntax startings recycling\n"
-- "  * findIgnoredFields  => given a sample S, report all max fields that don't change syntax\n" ++
-- "  * listDiffCtls => similar to listDiffs except ignores known register and constant buffer fields\n" ++
-- "      (uses opcode and ignores register and immediate differences)\n" ++
-- "   *

-- findIgnoredFields :: Sample -> IO ()
-- findIgnoredFields s = body
--  where body = do
--          ...


decode :: IO Sample -> IO ()
decode ms = do
  s <- ms
  disBitsRaw opts (sBits s) >>= putStrLn

listDiffs :: IO Sample -> IO Sample -> IO ()
listDiffs ms1 ms2 = do
  s1 <- ms1
  s2 <- ms2
  let fs = sampleDifferences s1 s2
  if null fs then putStrLn "all fields match"
    else do
      putStrLn $ intercalate "  " (map fmtFieldHeader fs)
      let fmt :: Sample -> Field -> String
          fmt s f = fmtFieldValue f (fGetValue f s)
      putStrLn $ intercalate "  " (map (fmt s1) fs)
      putStrLn $ intercalate "  " (map (fmt s2) fs)
-- given two samples, tell me the bits that are different
sampleDifferences :: Sample -> Sample -> [Field]
sampleDifferences (Sample w1) (Sample w2) = diffFieldsMatching 0 []
  where diffFieldsMatching :: Int -> [Field] -> [Field]
        diffFieldsMatching 128 rfs = reverse rfs
        diffFieldsMatching ix rfs
          | bitsMatch ix = diffFieldsMatching (ix+1) rfs
          | otherwise = diffFieldsMismatching (ix+1) ix rfs

        bitsMatch ix = getField128 ix 1 w1 == getField128 ix 1 w2

        diffFieldsMismatching 128 start_ix rfs = reverse ((start_ix,128 - start_ix):rfs)
        -- for now split at 64b boundary
        diffFieldsMismatching 64  start_ix rfs = diffFieldsMatching 64 ((start_ix,64 - start_ix):rfs)
        diffFieldsMismatching ix  start_ix rfs
          | ix == 64 || bitsMatch ix =
          diffFieldsMatching (ix+1) ((start_ix,ix - start_ix):rfs)
          | otherwise = diffFieldsMismatching (ix+1) start_ix rfs


fmtFieldHeader :: Field -> String
fmtFieldHeader f = padL (val_len`max`hdr_len) (fFormat f)
  where val_len = 2 + hexDigitsForLen (fLen f)
        hdr_len = length (fFormat f)
fmtFieldValue :: Field -> Word64 -> String
fmtFieldValue f v = padL hdr_len hex_str
  where hdr_len = length (fmtFieldHeader f)
        hex_str = printf ("0x%0" ++ show (hexDigitsForLen (fLen f)) ++ "X") v :: String

fmtFieldHeaderBinary :: Field -> String
fmtFieldHeaderBinary f = padL (val_len`max`hdr_len) (fFormat f)
  where val_len = 2 + (fLen f)
        hdr_len = length (fFormat f)
fmtFieldValueBinary :: Field -> Word64 -> String
fmtFieldValueBinary f v = padL hdr_len hex_str
  where hdr_len = length (fmtFieldHeader f)
        hex_str =
          "0b" ++ map (\ix -> if testBit v ix then '1' else '0') (reverse [0 .. fLen f - 1])

hexDigitsForLen :: Int -> Int
hexDigitsForLen len = (len + 3)`div`4

padL :: Int -> String -> String
padL k s = replicate (k - length s) ' ' ++ s
padR :: Int -> String -> String
padR k s = s ++ replicate (k - length s) ' '

data Sample = Sample {sBits :: !Word128}
  deriving (Eq,Ord,Show)


-- e.g. 000EE800001EE900`0000000006068381
sString :: String -> IO Sample
sString = return . sStringPure
sStringPure :: String -> Sample
sStringPure str =
  case reads ("0x" ++ str) of
    [(s,"")] -> Sample s
    _ -> error ("sFromBits: syntax error: " ++ show str)
sAllOps :: String -> IO [Sample]
sAllOps opname =
  sFile ("examples/" ++ oArch opts ++ "/ops/" ++ map toUpper opname++".sass")
sAllOpsK :: Int -> String -> IO [Sample]
sAllOpsK k opname = maybeTakePrefix <$> sAllOps opname
  where maybeTakePrefix = if k < 0 then id else take k
sDir :: FilePath -> IO [Sample]
sDir dir = do
  fs <- getSubPaths dir
  concat <$> mapM sFile fs
sFile :: FilePath -> IO [Sample]
sFile fp = do
  -- e.g. 000EE200001EED00`00001000020C7381:        LDG.E.128.SYS R12, [R2+0x10] {!1,+4.W} ; // examples/sm_75/samples\bodysystemcuda.sass:20925
  flns <- zip [1..] . lines <$> readFile fp
  let parseLine :: (Int,String) -> IO Sample
      parseLine (lno,lnstr) =
        case reads ("0x"++lnstr) of
          [(w128,_)] -> return (Sample w128)
          _ -> fail ("sLoadFile ("++fp++"): error on line " ++ show lno)
  mapM parseLine flns

type Field = (Int,Int)
fOff :: Field -> Int
fOff = fst
fLen :: Field -> Int
fLen = snd
fGetValue :: Field -> Sample -> Word64
fGetValue f = getField128 (fOff f) (fLen f) . sBits
fFormat :: Field -> String
fFormat f
  | fLen f == 1 = "[" ++ show (fOff f) ++ "]"
  | otherwise  = "[" ++ show (fOff f + fLen f - 1) ++ ":" ++ show (fOff f) ++ "]"


opts :: Opts
opts = dft_opts_75


type Row = ([(Field,Word64)],Sample)
type RowDis = (Row,String)

-- LDG.E.SYS R6, [R4] {!1,+3.W}
test_twiddle = twiddleField (79,2) (sString "000EA200001EE900`0000000004067381")

twiddleField :: Field -> IO Sample -> IO ()
twiddleField = twiddleFieldB
twiddleFields :: [Field] -> IO Sample -> IO ()
twiddleFields = twiddleFieldsB
twiddleFieldX :: Field -> IO Sample -> IO ()
twiddleFieldX f s = twiddleFields [f] s
twiddleFieldB :: Field -> IO Sample -> IO ()
twiddleFieldB f s = twiddleFieldsB [f] s
twiddleFieldsX :: [Field] -> IO Sample -> IO ()
twiddleFieldsX = twiddleFieldsBase False
twiddleFieldsB :: [Field] -> IO Sample -> IO ()
twiddleFieldsB = twiddleFieldsBase True
twiddleFieldsBase :: Bool -> [Field] -> IO Sample -> IO ()
twiddleFieldsBase binary fs s0_io =
  s0_io >>= twiddleFieldsBaseP binary fs
twiddleFieldsBaseP :: Bool -> [Field] -> Sample -> IO ()
twiddleFieldsBaseP binary fs s0 = body
  where body = do
          -- str0 <- disBitsRaw opts (sBits s)
          let (fmtHeader,fmtValue)
                | binary = (fmtFieldHeaderBinary,fmtFieldValueBinary)
                | otherwise = (fmtFieldHeader,fmtFieldValue)
          putStrLn $ intercalate " " (map ((++" ") . (" "++) . fmtHeader) fs)
          --
          let rows :: [Row]
              rows = map (\fvs -> (fvs,fieldValuesToSample fvs)) twiddle_cases

              fmtVal f w = chr ++ fmtValue f w ++ chr
                where chr = if fGetValue f s0 == w then "*" else " "

          -- disassemble en-mass
          strs <- disBitsRawBatch opts (map (sBits . snd) rows)
          forM_ (zip rows strs) $ \((fvs,_),syn) -> do
            putStrLn $
              intercalate "  " (map (uncurry fmtVal) fvs) ++ "  " ++ syn

        fieldValuesToSample :: [(Field,Word64)] -> Sample
        fieldValuesToSample = Sample . foldl' acc (sBits s0)
          where acc w128 (f,v) = putField128 (fOff f) (fLen f) v w128

        -- flatten the rows out so we can build a table of rows with samples
        -- and disassemble them en-mass
        twiddle_cases :: [[(Field,Word64)]]
        twiddle_cases = twids fs
          where twids :: [Field] -> [[(Field,Word64)]]
                twids [] = [[]]
                twids (f:fs) = concatMap prefixVal [0 .. (2^(fLen f)-1)]
                    where prefixVal :: Word64 -> [[(Field,Word64)]]
                          prefixVal v = map ((f,v):) fvs_tails

                          fvs_tails = twids fs

        -- twiddles :: [([(Field,Word64,Sample)],String)]

-- flatten and walk it flat
--        fields_with_vals :: [(Field,[(Word64,Sample)])]
--        fields_with_vals =
--          map (\f -> (f,map (overWriteWith f) [0 .. 2^(fLen f - 1)])) fs
--
--        derived_samples :: [[Sample]]
--        derived_samples = map (map overwriteWith) (map fieldWithVals fs)
--        overwriteWith :: (Field,Word64) -> Sample
--        overwriteWith (f,val) =
--          case s of
--            Sample w128 -> Sample $ putField128 (fOff f) (fLen f) val w128


extractFields :: IO [Sample] -> [Field] -> IO ()
extractFields io_ss fs = body
  where body = do
          ss <- io_ss
          putStrLn "disassembling"
          strs <- disBitsRawBatch opts (map sBits ss)
          --
          let binary = True
          let (fmtHeader,fmtValue)
                | binary = (fmtFieldHeaderBinary,fmtFieldValueBinary)
                | otherwise = (fmtFieldHeader,fmtFieldValue)
          putStrLn $ intercalate "  " (map fmtHeader fs)
          --
          let fmtVal f w = " " ++ fmtValue f w
          --
          forM_ (zip ss strs) $ \(s,str) -> do
            let fvs = map (\f -> (f,getField128 (fOff f) (fLen f) (sBits s))) fs
            putStrLn $
              intercalate "  " (map (uncurry fmtValue) fvs) ++ "  " ++ str


longestCommonFields :: IO [Sample] -> IO ()
longestCommonFields io_ss = body
  where body = do
          ss <- io_ss
          putStrLn $ show (length ss) ++ " samples"
          find ss

        find ss = printPattern 127
          where w128s = map sBits ss

                printPattern :: Int -> IO ()
                printPattern ix = do
                  let bs = map (\w128 -> getField128 ix 1 w128) w128s
                  if all (==head bs) bs then putStr (show (head bs))
                    else putStr "*"
                  when (ix == 64) $
                    putStr "`"
                  when (ix > 0) $
                    printPattern (ix-1)
                  when (ix == 0) $
                    putStrLn ""


createPattern :: IO [Sample] -> IO String
createPattern io_ss = body
  where body = io_ss >>= mkStr . map sBits

        mkStr w128s = bit 0 ""
          where bit 128 rbits = return (reverse rbits)
                bit ix  rbits = bit (ix+1) (chr:rbits)
                  where bs = map (\w128 -> getField128 ix 1 w128) w128s
                        chr
                          | all (==head bs) bs = if head bs == 0 then '0' else '1'
                          | otherwise = '*'


listOps :: [String] -> IO ()
listOps = mapM_ (\s -> putStrLn s >> longestCommonFields (sAllOps s))

listOps_LDX :: IO ()
listOps_LDX = listOps ["LD","LDC","LDG","LDL","LDS","LDSM"]


opcodeTest :: IO ()
opcodeTest = do
  ps <- getSubPaths "examples/sm_75/ops"
  -- let ops = map (dropExtension . takeFileName) ps
  pats <- mapM (createPattern . sFile) ps
  let prefixLen = length . takeWhile (/='*')
  mapM_  (\(p,pat) -> putStrLn (printf "%5d " (prefixLen pat) ++ pat ++ "  " ++ dropExtension (takeFileName p))) (zip ps pats)






