module Analysis where

import Sample
import NVT.Bits
import NVT.CUDASDK
import NVT.Opts
import NVT.RawInst


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




-- FIXME:
--   if start bit is the top part of another field, will this cause confusion

probeReserved :: Int -> IO [Sample] -> IO ()
probeReserved ix io_ss = io_ss >>= body
  where body :: [Sample] -> IO ()
        body ss = do
          lens <- mapM startProbe ss
          let min_len = minimum (0:lens)
          if min_len == 0 then putStrLn "field has syntactic meaning for at least one sample"
            else putStrLn $ "===> " ++ fFormat (ix,min_len) ++ ": is probably reserved <==="

        startProbe :: Sample -> IO Int
        startProbe s = do
          [str0,str1] <- disassembleField (ix,1) s
          if str0 == str1 then extendFieldMbz s 2
            else return 0
        extendFieldMbz :: Sample -> Int -> IO Int
        extendFieldMbz s len = do
          -- putStrLn $ "extend field MBZ: " ++ show len
          strs <- disassembleField (ix,len) s
          if all (==head strs) strs then extendFieldMbz s (len+1)
            else return (len-1)

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
    "  listDiffs           => lists the different fields between two samples\n" ++
    "  surveyFields        => lists the values of a given set of fields\n" ++
    "  decode              => emits the syntax for a sample\n" ++
    "  twiddleField[s][BX] => given field[s], test all values and emit syntax\n" ++
    "                         emit data in binary or hex (or omit for default)\n" ++
    "  findValues         => finds longest fields that match each input in the sequence of samples\n" ++
    "\n" ++
    "SAMPLE CONSTRUCTORS:\n" ++
    "  sString           => loads a bit pattern from a `-separated Word128 string\n" ++
    "  sFile[K]          => loads samples from a file (sFile \"foo.sass\")\n" ++
    "  sAllOps[K]        => loads all ops with a given name (sAllOps \"LDC\")\n" ++
    "SAMPLE MODIFIERS:\n" ++
    "  * pre-twiddle a sample field\n" ++
    "    ~> F .= VAL\n" ++
    "    (sString ... ~> (9,3).=5) flips field [11:9] to be 5\n" ++
    "\n" ++
    "  * pre-twiddle several sample fields\n" ++
    "    *~> [F .= VAL, ...]\n" ++
    "    (sString ... *~> [(91,1).=1, (9,3).=5]) flips field [11:9] to be 5 and [91] to be 1\n" ++
    "\n" ++
    "EXAMPLES:\n" ++
    "  Lists the field values in from the first 100 samples in the given files.\n" ++
    "    *> surveyFields [(90,2),(9,3)] (sFileK 100 \"examples/sm_75/ops/LDL.sass\")\n" ++
    "\n" ++
    "  Similar to the previous, but lookup the ISA file by op name\n" ++
    "    *> surveyFields [(90,2),(9,3)] (sAllOpsK 24 \"LDC\")\n" ++
    "\n" ++
    "  List differences between two ops and then interpolate field by field\n" ++
    "    *> listDiffs (sString \"000EA20000000000`00006900FF0F7B82\") (sString \"000E620000000000`00006440FF037B82\")\n" ++
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


decode :: IO [Sample] -> IO ()
decode ms = do
  (s:_) <- ms
  disBitsRaw opts (sBits s) >>= putStr
  putStrLn $ "  " ++ "{" ++ intercalate "," (decodeDepInfo (sBits s)) ++ "}"
decodeR :: IO [Sample] -> IO ()
decodeR ms = do
  (s:_) <- ms
  disBitsRaw opts (sBits s) >>= putStrLn

listDiffs :: IO [Sample] -> IO [Sample] -> IO ()
listDiffs ms1 ms2 = do
  (s1:_) <- ms1
  (s2:_) <- ms2
  let fs = sampleDifferences s1 s2
  if null fs then putStrLn "all fields match"
    else do
      putStrLn $ intercalate "  " (map fmtFieldHeader fs)
      let fmt :: Sample -> Field -> String
          fmt s f = fmtFieldValue f (fGetValue f s)
      s1_str <- disBitsRaw opts (sBits s1)
      putStrLn $ intercalate "  " (map (fmt s1) fs) ++ "  " ++ s1_str
      s2_str <- disBitsRaw opts (sBits s2)
      putStrLn $ intercalate "  " (map (fmt s2) fs) ++ "  " ++ s2_str
      putStrLn "INTERPOLATING"
      let interpolateField :: Field -> IO ()
          interpolateField f = do
            let val2 = fGetValue f s2
                s1a = Sample (putField128 (fOff f) (fLen f) val2 (sBits s1))
            s1a_str <- disBitsRaw opts (sBits s1a)
            putStrLn $ intercalate "  " (map (fmt s1a) fs) ++ "  (flipped " ++ show f ++ ") " ++ s1a_str


      mapM_ interpolateField fs



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


{-
listFields :: [Field] -> IO [Sample] -> IO ()
listFields fs io_ss = do
    ss <- io_ss
    forM_ ss $ \ss -> do
      decode
      putStrLn fmtSs ss
  where
        fmtSs :: Sample -> IO String
        fmtSs s = intercalate " " $ map fmtField fs ++ "\n"
-}

-- type RowDis = (Row,String)

-- LDG.E.SYS R6, [R4] {!1,+3.W}
-- test_twiddle = twiddleField (79,2) (sString "000EA200001EE900`0000000004067381")
type TwiddleRow = ([(Field,Word64)],[Sample])


twiddleField :: Field -> IO [Sample] -> IO ()
twiddleField = twiddleFieldB
twiddleFields :: [Field] -> IO [Sample] -> IO ()
twiddleFields = twiddleFieldsB
twiddleFieldX :: Field -> IO [Sample] -> IO ()
twiddleFieldX f = twiddleFields [f]
twiddleFieldB :: Field -> IO [Sample] -> IO ()
twiddleFieldB f = twiddleFieldsB [f]
twiddleFieldsX :: [Field] -> IO [Sample] -> IO ()
twiddleFieldsX = twiddleFieldsBase False
twiddleFieldsB :: [Field] -> IO [Sample] -> IO ()
twiddleFieldsB = twiddleFieldsBase True
twiddleFieldsBase :: Bool -> [Field] -> IO [Sample] -> IO ()
twiddleFieldsBase binary fs s0_io =
  s0_io >>= twiddleFieldsBaseP binary fs
twiddleFieldsBaseP :: Bool -> [Field] -> [Sample] -> IO ()
twiddleFieldsBaseP binary fs s0s = body
  where body = do
          -- str0 <- disBitsRaw opts (sBits s)
          let (fmtHeader,fmtValue)
                | binary = (fmtFieldHeaderBinary,fmtFieldValueBinary)
                | otherwise = (fmtFieldHeader,fmtFieldValue)
          putStrLn $ intercalate " " (map ((++" ") . (" "++) . fmtHeader) fs)
          --
          let rows :: [TwiddleRow]
              rows = map (\fvs -> (fvs,fieldValuesToSamples fvs)) twiddle_cases

              fmtVal :: Bool -> Sample -> Field -> Word64 -> String
              fmtVal first s0 f w = maybe_star ++ middle ++ maybe_star
                where maybe_star = if fGetValue f s0 == w then "*" else " "
                      middle
                        | first = str
                        | otherwise = replicate (length str) ' '
                        where str = fmtValue f w

          -- disassemble en-mass
          -- strs <- disBitsRawBatch opts (map (sBits . snd) rows)
          let chunk _ [] = []
              chunk k as =
                case splitAt k as of
                  (pfx,sfx) -> pfx : chunk k sfx
          syns_chunks <- chunk (length s0s) <$> disBitsRawBatch opts (concatMap (map sBits . snd) rows)
          -- mapM_ putStrLn (concat strs)
          -- strs is [[String]]
          forM_ (zip rows syns_chunks) $ \((fvs,mod_ss),mod_ss_syns) -> do
            let fmtRowEntry :: (Int,Sample,Sample,String) -> String
                fmtRowEntry (i,s0,s_m,syn) =
                    values ++
                    " " ++ drop (length "0x") (show (sBits s_m)) ++  -- bits
                    "  " ++ syn ++
                    "\n"
                  where values = intercalate "  " (map (uncurry (fmtVal (i==0) s0)) fvs)
            putStr $
              concatMap fmtRowEntry
                (zip4 [0..] s0s mod_ss mod_ss_syns)

--        fmtRow :: Row -> String
--        fmtRow (fvs,ss)

        fieldValuesToSamples :: [(Field,Word64)] -> [Sample]
        fieldValuesToSamples fvs = map (\s0 -> Sample (foldl' acc (sBits s0) fvs)) s0s
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


surveyFields :: [Field] -> IO [Sample] -> IO ()
surveyFields fs io_ss = body
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
          -- vs :: [[(Field,Word64)]] (each row)
          fvs_rows <-
            forM (zip ss strs) $ \(s,str) -> do
              let fvs :: [(Field,Word64)]
                  fvs = map (\f -> (f,getField128 (fOff f) (fLen f) (sBits s))) fs
              putStrLn $
                intercalate "  " (map (uncurry fmtValue) fvs) ++ "  " ++ str
              return fvs
          let reorderToCols :: [[(Field,Word64)]] -> [(Field,[Word64])]
              reorderToCols = zipWith (\f col -> (f,map snd col)) fs . transpose

              freqOn :: [Word64] -> Word64 -> Int
              freqOn ws v = length $ filter (==v) ws

              toHist :: [Word64] -> [(Word64,Int)]
              toHist ws = doIt ws
                where freq = freqOn ws
                      doIt =
                        reverse .                    -- highest to lowest
                          sortOn snd .               -- order by freq
                            map (\v -> (v,freq v)) . -- count it (pairing with value)
                              nubOrd                 -- for each unique value

              fmtFieldHistogram :: (Field,[Word64]) -> String
              fmtFieldHistogram (f,vs) =
                  fmtFieldHeader f ++ " =\n" ++
                  concatMap fmtH h
                where h = toHist vs
                      fmtH (v,n) =
                        "  " ++ fmtFieldValueBinary f v ++ " => " ++
                        printf "%5d" n ++ "    " ++ baysian v ++ "\n"
                      --
                      baysian v = intercalate ", " (map fmtSubHist baysian_data)
                        where rows_with_this_v = filter rowHasV fvs_rows
                                where rowHasV :: [(Field,Word64)] -> Bool
                                      rowHasV row = find (\(f1,v1) -> f1 == f && v1 == v) row /= Nothing
                              -- kick f's column out of the data
                              baysian_data :: [(Field,[Word64])]
                              baysian_data = filter ((/=f) . fst) (reorderToCols rows_with_this_v)

                              fmtSubHist :: (Field,[Word64]) -> String
                              fmtSubHist (f,vs) =
                                  " => " ++ fmtFieldHeader f ++ hist_str
                                where vs_dom = nubOrd vs
                                      showFreq v = fmtFieldValueBinary f v ++ ":" ++ show (freqOn vs v)
                                      hist_str
                                        | length vs_dom == 1 = " always " ++ show (head vs_dom)
                                        | otherwise = " ~ {" ++ intercalate ", " (map showFreq vs_dom) ++ "}"
          --
          putStrLn $
            concatMap (fmtFieldHistogram) (reorderToCols fvs_rows)
          return ()

-- use natural order (we could maintain appearance order)
nubOrd :: Ord a => [a] -> [a]
nubOrd = map fst . DM.toList . DM.fromList . map (\a -> (a,()))

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




------------------------------------------------------------
-- The goal is to emit formats by looking at initial syntax
-- Come up with a basic parser.
--   data InstMid
--
--
-- extract instruction options
-- discover register files
-- see if it supports constant buffers
-- if it's ternary, it behaves differently
--
-- studyInstruction :: IO [Sample] -> IO ()
-- studyInstruction =
--   parse all the instructions to my IR
--   determine if they are:
--     nullary
--     unary
--     ternary
--
