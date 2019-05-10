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



studyOpcodes :: IO ()
studyOpcodes = studyOpcodesStartingAt 0x000

studyOpcodesStartingAt :: Word64 -> IO ()
studyOpcodesStartingAt at = do
    writeFile "ops.txt" ""
    writeFile "log.txt" ""
    writeFile "defs.txt" ""
    tryOpcodes False [at .. 0x1FF]
  where tryOpcodes :: Bool -> [Word64] -> IO ()
        tryOpcodes _ [] = return ()
        tryOpcodes last_was_valid (op:ops) = do
            putStrLn "=============================="
            appendL $
              printf "OPCODE 0x%03X" op ++ " " ++
              fmtFieldValueBinary (0,9) op ++ "  " ++ enc128 ++ "\n"

            tryRegFiles
            -- 000FE80000005800`0000000009027984:        LDS.U R2, [R9.X4] {!4} ;               // examples/sm_75/libs\cudnn64_7-cudnn64_7.776.sm_75.sass:69944
            -- let lds_bits = Word128 0x000FE80000000000 0x0000000000000984
          where template_bits = Word128 0x000FE80000000000 0x0000000000000000
                bits_with_op = putField128 0 10 op template_bits
                enc128 = drop 2 (show bits_with_op)
                appendS :: String -> IO ()
                appendS = appendFile "ops.txt"
                appendL :: String -> IO ()
                appendL s = do
                  appendFile "log.txt" s
                  putStr s
                appendD :: String -> IO ()
                appendD = appendFile "defs.txt"

                badAsm str = "nvdisasm error"`isInfixOf`str

                tryRegFiles = do
                  let bitss :: [Word128]
                      bitss = map (uncurry mkBits) all_cases
                        where mkBits b91 b12_9 =
                                putField128 91 1 b91 (putField128 9 3 b12_9 bits_with_op)
                              all_cases = [(b91,b12_9) | b91<-[0..1], b12_9<-[0..7]]
                  cases <-
                    zip bitss <$> disBitsRawBatch opts bitss
                  let good_cases :: [(Word128,String)] -- RF paired with syntax
                      good_cases = filter (not . badAsm . snd) cases
                      fmtCase (w128,str) =
                        printf "  %-12s"
                          (show (getField128 91 1 w128) ++ "x" ++ show (getField128 9 3 w128)) ++
                          " ==> " ++ str ++ "\n"
                  appendL $ concatMap fmtCase good_cases

                  let findAliased :: String -> [Word64]
                      findAliased s
                        --
                        -- OPCODE 0x000 0b000000000
                        --  0 ==> NOP
                        -- OPCODE 0x118 0b100011000
                        --  4 ==> @P0 NOP
                        | s == "NOP" = [0x000,0x118]
                        --
                        -- No official examples of PLOP3
                        -- The first is probably the most common.
                        --
                        -- OPCODE 0x01C 0b000011100
                        --   4 ==> @P0 PLOP3.LUT P0, P0, P0, P0, P0, 0x0, 0x0
                        -- OPCODE 0x01D 0b000011101
                        --   1 ==> @P0 PLOP3.LUT P0, P0, P0, R0.SIGN, P0, 0x0, 0x0
                        --   5 ==> @P0 PLOP3.LUT P0, P0, P0, c[0x0] [0x0].SIGN, P0, 0x0, 0x0
                        -- OPCODE 0x01E 0b000011110
                        --   1 ==> @P0 PLOP3.LUT P0, P0, P0, R0.SIGN, R0.SIGN, 0x0, 0x0
                        --   5 ==> @P0 PLOP3.LUT P0, P0, P0, c[0x0] [0x0].SIGN, R0.SIGN, 0x0, 0x0
                        -- OPCODE 0x01F 0b000011111
                        --   1 ==> @P0 PLOP3.LUT P0, P0, R0.SIGN, R0.SIGN, R0.SIGN, 0x0, 0x0
                        --   5 ==> @P0 PLOP3.LUT P0, P0, R0.SIGN, c[0x0] [0x0].SIGN, R0.SIGN, 0x0, 0x0
                        | s == "PLOP3" = [0x01C,0x01D,0x01E,0x01C] -- take the least exotic
                        --
                        -- OPCODE 0x024 0b000100100
                        --   1 ==> @P0 IMAD.U32 R0, R0, R0, R0
                        --   2 ==> @P0 IMAD.U32 R0, R0, R0, 0x0
                        --   3 ==> @P0 IMAD.U32 R0, R0, R0, c[0x0][0x0]
                        --   4 ==> @P0 IMAD.MOV.U32 R0, R0, 0x0, R0
                        --   5 ==> @P0 IMAD.U32 R0, R0, c[0x0][0x0], R0
                        -- OPCODE 0x025 0b000100101
                        --   1 ==> @P0 IMAD.WIDE.U32 R0, P0, R0, R0, R0
                        --   3 ==> @P0 IMAD.WIDE.U32 R0, P0, R0, R0, c[0x0][0x0]
                        --   4 ==> @P0 IMAD.WIDE.U32 R0, P0, R0, 0x0, R0
                        --   5 ==> @P0 IMAD.WIDE.U32 R0, P0, R0, c[0x0][0x0], R0
                        | s == "IMAD" = [0x024,0x025] -- second one is wide
                        --
                        -- OPCODE 0x036 0b000110110
                        --   1 ==> @P0 HMMA.884.F16.F16.STEP0 R0, R0.ROW, R0.ROW, R0
                        -- OPCODE 0x03C 0b000111100
                        --   1 ==> @P0 HMMA.1688.F16 R0, R0, R0, R0
                        -- OPCODE 0x03D 0b000111101
                        --   1 ==> @P0 BMMA.88128.XOR.???0 R0, R0.ROW, R0.???0, R0
                        | s == "HMMA" = [0x036,0x03C]
                        --
                        -- OPCODE 0x143 0b101000011
                        --  1 ==> @P0 CALL.ABS P0, R0
                        --  4 ==> @P0 CALL.ABS P0, 0x0
                        --  5 ==> @P0 CALL.ABS P0, c[0x0][0x0]
                        -- OPCODE 0x144 0b101000100
                        --  1 ==> @P0 CALL.REL P0, R0 0x10
                        --  4 ==> @P0 CALL.REL P0, 0x10
                        | s == "CALL" = [0x143,0x144]
                        --
                        -- OPCODE 0x152 0b101010010
                        --   1 ==> @P0 RPCMOV.32 Rpc.LO, R0
                        --   4 ==> @P0 RPCMOV.32 Rpc.LO, 0x0
                        --   5 ==> @P0 RPCMOV.32 Rpc.LO, c[0x0][0x0]
                        -- OPCODE 0x153 0b101010011
                        --   1 ==> @P0 RPCMOV.32 R0, Rpc.LO
                        -- OPCODE 0x154 0b101010100
                        --   4 ==> @P0 RPCMOV.64 Rpc, 0x0
                        --   5 ==> @P0 RPCMOV.64 Rpc, c[0x0][0x0]
                        | s == "RPCMOV" = [0x153,0x154]
                        --
                        -- OPCODE 0x155 0b101010101
                        --  1 ==> @P0 BMOV.32 R0, B0
                        -- OPCODE 0x156 0b101010110
                        --   1 ==> @P0 BMOV.32 B0, R0
                        --   4 ==> @P0 BMOV.32 B0, 0x0
                        --   5 ==> @P0 BMOV.32 B0, c[0x0][0x0]
                        --   7 ==> @P0 BMOV.32 B0, B0
                        -- OPCODE 0x157 0b101010111
                        --   1 ==> @P0 BMOV.64 ATEXIT_PC, R0
                        --   4 ==> @P0 BMOV.64 ATEXIT_PC, 0x0
                        --   5 ==> @P0 BMOV.64 ATEXIT_PC, c[0x0][0x0]
                        | s == "BMOV" = [0x153,0x154]
                        --
                        -- OPCODE 0x18A 0b110001010
                        --   1 ==> @P0 ATOM.ADD.EF.INVALID0.CTA P0, R0, [R0], R0
                        -- OPCODE 0x18B 0b110001011
                        --   1 ==> @P0 ATOM.CAS.EF.INVALID0.CTA P0, R0, [R0], R0, R0
                        | s == "ATOM" = [0x18C,0x18D]
                        --
                        -- OPCODE 0x18C 0b110001100
                        --   1 ==> @P0 ATOMS.ADD R0, [R0], R0
                        -- OPCODE 0x18D 0b110001101
                        --   1 ==> @P0 ATOMS.CAS R0, [R0], R0, R0
                        | s == "ATOMS" = [0x18C,0x18D]
                        --
                        -- OPCODE 0x193 0b110010011
                        --   2 ==> @P0 SUATOM.D.1D.ADD.EF.INVALID0.CTA.IGN P0, R0, [R0], R0, 0x0, 0x0
                        --   3 ==> @P0 SUATOM.D.1D.ADD.EF.INVALID0.CTA.IGN P0, R0, [R0], R0, 0x0, 0x0, 0x0
                        -- OPCODE 0x194 0b110010100
                        --   1 ==> @P0 SUATOM.D.1D.ADD.EF.INVALID0.CTA.IGN P0, R0, [R0], R0, R0
                        -- OPCODE 0x195 0b110010101
                        --   2 ==> @P0 SUATOM.D.1D.CAS.EF.INVALID0.CTA.IGN P0, R0, [R0], R0, 0x0, 0x0
                        --   3 ==> @P0 SUATOM.D.1D.CAS.EF.INVALID0.CTA.IGN P0, R0, [R0], R0, 0x0, 0x0, 0x0
                        -- OPCODE 0x196 0b110010110
                        --   1 ==> @P0 SUATOM.D.1D.CAS.EF.INVALID0.CTA.IGN P0, R0, [R0], R0, R0
                        | s == "SUATOM" = [0x193 .. 0x196]
                        --
                        -- OPCODE 0x197 0b110010111
                        --   3 ==> @P0 SULD.P.1D.EF.INVALID0.CTA.INVALID0.IGN P0, R0, [R0], 0x0, 0x0, 0x0
                        --   5 ==> @P0 SULD.P.1D.EF.INVALID0.CTA.INVALID0.IGN P0, R0, [R0], 0x0, 0x0
                        -- OPCODE 0x198 0b110011000
                        --   4 ==> @P0 SULD.P.1D.EF.INVALID0.CTA.INVALID0.IGN P0, R0, [R0], R0
                        -- OPCODE 0x199 0b110011001
                        --   3 ==> @P0 SULD.D.1D.EF.U8.INVALID0.CTA.IGN P0, R0, [R0], 0x0, 0x0, 0x0
                        --   5 ==> @P0 SULD.D.1D.EF.U8.INVALID0.CTA.IGN P0, R0, [R0], 0x0, 0x0
                        -- OPCODE 0x19A 0b110011010
                        --   4 ==> @P0 SULD.D.1D.EF.U8.INVALID0.CTA.IGN P0, R0, [R0], R0
                        | s == "SULD" = [0x197 .. 0x19A]
                        --
                        -- OPCODE 0x19B 0b110011011
                        --   3 ==> @P0 SUST.P.1D.EF.INVALID0.CTA.INVALID0.IGN [R0], R0, 0x0, 0x0, 0x0
                        --   5 ==> @P0 SUST.P.1D.EF.INVALID0.CTA.INVALID0.IGN [R0], R0, 0x0, 0x0
                        -- OPCODE 0x19C 0b110011100
                        --   4 ==> @P0 SUST.P.1D.EF.INVALID0.CTA.INVALID0.IGN [R0], R0, R0
                        -- OPCODE 0x19D 0b110011101
                        --   3 ==> @P0 SUST.D.1D.EF.U8.INVALID0.CTA.IGN [R0], R0, 0x0, 0x0, 0x0
                        --   5 ==> @P0 SUST.D.1D.EF.U8.INVALID0.CTA.IGN [R0], R0, 0x0, 0x0
                        -- OPCODE 0x19E 0b110011110
                        --   4 ==> @P0 SUST.D.1D.EF.U8.INVALID0.CTA.IGN [R0], R0, R0
                        | s == "SUST" = [0x19B .. 0x19E]
                        --
                        -- OPCODE 0x19F 0b110011111
                        --   2 ==> @P0 SURED.D.1D.ADD.EF.INVALID0.CTA.IGN [R0], R0, 0x0, 0x0
                        --   3 ==> @P0 SURED.D.1D.ADD.EF.INVALID0.CTA.IGN [R0], R0, 0x0, 0x0, 0x0
                        -- OPCODE 0x1A0 0b110100000
                        --   1 ==> @P0 SURED.D.1D.ADD.EF.INVALID0.CTA.IGN [R0], R0, R0
                        | s == "SURED" = [0x19F,0x1A0]
                        --
                        -- OPCODE 0x1A8 0b110101000
                        --   1 ==> @P0 ATOMG.ADD.EF.INVALID0.CTA P0, R0, [R0], R0
                        -- OPCODE 0x1A9 0b110101001
                        --   1 ==> @P0 ATOMG.CAS.EF.INVALID0.CTA P0, R0, [R0], R0, R0
                        | s == "ATOMG" = [0x1A8,0x1A9]
                        | otherwise = []

                  was_valid <-
                    case good_cases of
                      ((w128,syn):_)
                        | null (findAliased mne) || take 1 (findAliased mne) == [op] -> do
                          appendS "\n"
                          appendS $ "  | Op" ++ mne
                          emitDef all_encs mne good_cases
                          return True
                        where all_encs = case findAliased mne of {[] -> [op]; ops -> ops}
                              mne =
                                case dropWhile ("@"`isPrefixOf`) (words syn) of
                                  (mne:_) -> takeWhile (/='.') mne
                      _ -> do
                        when last_was_valid $
                          appendS "\n"
                        appendS $ " | " ++ printf "OpINVALID_0x%X" op
                        return False
                  putStrLn ""
                  tryOpcodes was_valid ops

                emitDef :: [Word64] -> String -> [(Word128,String)] -> IO ()
                emitDef all_encs mne good_cases = do
                    str_lat <- findLatency
                    appendD $
                      " , mk  " ++ intercalate " " [
                                      str_mne
                                    , str_encs
                                    , str_fmts
                                    , str_lat
                                    , str_pipe
                                    , str_attrs
                                    ]
                     ++ "\n"
                  where str_mne = printf "%-12s" ("Op" ++ mne)
                        str_encs = printf "%-12s" ("[" ++ intercalate "," (map (printf "0x%03X") all_encs) ++ "]")
                        str_fmts = "..formats.."

                        findLatency :: IO String
                        findLatency = do
                          putStrLn "  finding latency ...."
                          let f = "examples/sm_75/ops/" ++ mne ++ ".sass"
                          z <- doesFileExist f
                          if not z then do
                              putStrLn "     => no samples: using hardcoded latency"
                              return useHardcodedLatency
                            else do
                              let processLns :: Bool -> Bool -> Int -> [String] -> IO String
                                  processLns uses_barrier always_yield longest [] = do
                                    putStrLn $ "  longest observed stall count is " ++ show longest
                                    if uses_barrier then return "OpLatencyVariable"
                                      else return ("(OpLatencyFixed " ++ show longest ++ ")")
                                  processLns uses_barrier always_yield longest (ln:lns) = do
                                    let badLine why = do
                                          putStrLn $ why ++ ": BAD LATENCY LINE: " ++ ln
                                          processLns uses_barrier always_yield longest lns
                                    case span (/=':') ln of
                                      (_,"") -> badLine "no :"
                                      (pfx,':':_) ->
                                        case reads ("0x"++pfx) :: [(Word128,String)] of
                                          [(w128,"")] -> do
                                            let new_longest = longest `max` (fromIntegral (getField128 105 4 w128))
                                                new_yield = always_yield && getField128 109 1 w128 == 0
                                                new_barrier = uses_barrier ||
                                                  getField128 110 3 w128 /= 7 ||
                                                  getField128 113 3 w128 /= 7
                                            processLns new_barrier new_yield new_longest lns
                                          _ -> badLine "no reads"

                              lns <- take 10000 . lines <$> readFile f
                              processLns False True (-1) lns

                        str_pipe
                          | flt64 || flt16 || flt32 = "OP_FLT"
                          | int = "OP_INT"
                          | tpu = "OP_TPU"
                          | load || store || surface || texture || atomic || mne`elem`["SHFL"] = "OP_LSU"
                          | uniform = "OP_UNI"
                          | otherwise = "OP_UNK"
                        useHardcodedLatency
                          | is_var = "OpLatencyVariable"
                          | otherwise = "(OpLatencyFixed (-1))"
                          where is_var =
                                    slm || load || store || atomic ||
                                    surface || texture || branch ||
                                    flt64 || flt16 ||
                                    other_var_op
                                other_var_op = mne`elem`["SHLF","PRMT","S2R"]
                                -- TODO: look up based on samples
                        str_attrs = "[" ++ intercalate "," (map ("OpAttr"++) attrs) ++ "]"
                          where syntaxHas :: String -> Bool
                                syntaxHas tok = any (`isInfixOf`tok) (map snd good_cases)
                                attrs =
                                     cond (syntaxHas "@") "PREDICATED"
                                  ++ cond slm "SHMEM"
                                  ++ cond movement "MOVEMENT"
                                  ++ cond flt16 "FLT16"
                                  ++ cond flt32 "FLT32"
                                  ++ cond flt64 "FLT64"
                                  ++ cond int "INT"
                                  ++ cond bitwise "BITWISE"
                                  ++ cond setp "SETP"
                                  ++ cond load "LOAD"
                                  ++ cond store "STORE"
                                  ++ cond surface "SURFACE"
                                  ++ cond texture "TEXTURE"
                                  ++ cond branch "BRANCH"
                                  ++ cond tpu "TPU"
                                  ++ cond uniform "UNIFORM"

                        cond :: Bool -> a -> [a]
                        cond z a = if z then [a] else []

                        slm :: Bool
                        slm = mne `elem` ["LDS","LDSM","STS"]
                        bitwise = mne`elem`["LOP3","PLOP3","SHL","SHR","SHF","FLO","BREV","POPC"]
                        logical = mne`elem`["LOP3","PLOP3","UPLOP3"]
                        flt16 :: Bool
                        flt16 = mne `elem` ["HADD2","HFMA2","HMUL2","HSET2","HSETP2"]
                        flt32 :: Bool
                        flt32 = mne `elem` ["F2I","F2FP","FADD","FFMA","FCHK","FMNMX","FMUL","FSEL","FSET","FSETP","FSWZADD","MUFU"]
                        flt64 :: Bool
                        flt64 = mne `elem` ["DADD","DFMA","DMUL","DSETP","MUFU"]
                        int :: Bool
                        int = mne `elem` ["SGXT","LEA","I2I","I2F","IABS","IADD3","IDP","IMAD","IMMA","IMNMX","ISETP"]
                        setp :: Bool
                        setp = mne `elem` ["ISETP","FSET","FSETP","DSETP"]
                        load :: Bool
                        load = mne `elem` ["LD","LDC","LDG","LDL","LDS","LDSM"]
                        store :: Bool
                        store = mne `elem` ["ST","STG","STL","STS"]
                        atomic :: Bool
                        atomic = mne `elem` ["ATOM","ATOMG","ATOMS","RED","SUATOM"]
                        surface :: Bool
                        surface = mne `elem` ["SUATOM","SULD","SUST","SURED"]
                        texture :: Bool
                        texture = mne `elem` ["TEX","TLD","TLD4","TLD","TMML","TXD","TXQ"]
                        branch :: Bool
                        branch = mne `elem` ["BPT","BRA","BRX","CALL","KILL","NANOTRAP","RET","RTT"]
                        movement :: Bool
                        movement = mne `elem` ["MOV","MOVM","SHFL","PRMT","P2R","S2R"]
                        tpu = mne `elem` ["HMMA","BMMA","IMMA"]
                        uniform = mne `elem` ["ULDC","UPLOP3","S2UR"]



studyDependencies :: Word64 -> FilePath -> IO ()
studyDependencies op sample_file =
  -- load file
  -- for each instance with op (op)
  --   adhoc find dst and preds written
  --   scan ahead up to K ops for consumer
  --   analyze dependency (stalls+yield)
  return ()

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
