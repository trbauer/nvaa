module RunBin where

import NVT.Bits
import NVT.Diagnostic
import NVT.Encoders.InstEncoder
import NVT.IR
import NVT.Loc
import NVT.Parsers.ListingParser


import Control.Monad
import Control.Concurrent(forkIO)
import Control.Concurrent.MVar
import Data.List
import Data.Bits
import Data.Word
import Data.Int
import Debug.Trace
import System.Environment
import System.Exit
import System.IO
import System.Directory
import System.Process
import Text.Printf
import qualified Data.ByteString as BS

driver_skeleton :: FilePath
-- driver_skeleton = "it/micro.exe"
-- driver_skeleton = "micro_skeleton.cubin"
driver_skeleton = "tools/runbin-skeleton.exe"

patched_exe :: FilePath
patched_exe = "runbin-patched.exe"
-- patched_exe = "patched.cubin"

main :: IO ()
main = getArgs >>= run

data Opts =
  Opts {
    oVerbosity :: !Int
  , oSaveExe :: !Bool
  , oInputPath :: !FilePath
  , oIterations :: !Int
  --
  , oSrc0s :: !(Maybe Int32)
  , oSrc1s :: !(Maybe Int32)
  , oSrc2 :: !(Maybe Int32)
  , oTpb :: !(Maybe Int32)
  , oBs :: !(Maybe Int32)
  --
  , oPreloadRegisters :: !Bool
  } deriving Show
osVerboseLn :: Opts -> String -> IO ()
osVerboseLn os = when (oVerbosity os > 0) . putStrLn

dft_opts :: Opts
dft_opts =
  Opts {
    oVerbosity = 0
  , oSaveExe = False
  , oInputPath = ""
  , oIterations = 1
  --
  , oSrc0s = Nothing
  , oSrc1s = Nothing
  , oSrc2 = Nothing
  , oBs = Nothing
  , oTpb = Nothing
  , oPreloadRegisters = False
  }

run :: [String] -> IO ()
run as = parseArgs dft_opts as >>= runWithOpts (const True)


vERSION :: String
vERSION = "0.0.1"

usage :: String
usage =
  "Run Assembly (" ++ vERSION ++ ")\n" ++
  "\n" ++
  "usage: runasm.exe [OPTIONS]  <INPUT>\n" ++
  "where OPTIONS are:\n" ++
  "  -v/-v2/-q                     verbose/debug/quiet\n" ++
  "  -i=INT                        iterations\n" ++
  "\n" ++
  "  -s0s=INT                      src0's values (variable)\n" ++
  "  -s1s=INT                      src1's value (variable)\n" ++
  "  -s2=INT                       src2's value (uniform)\n" ++
  "  -bs=INT                       number of blocks\n" ++
  "  -tpb=INT                      number of threads per block\n" ++
  "\n" ++
  "  -pr/--preload-registers       controls if we should preload inputs\n" ++
  "  -npr/--no-preload-registers     R7  - holds src0[ID]\n" ++
  "                                  R8  - holds src1[ID]\n" ++
  "                                  R9  - holds src2\n" ++
  "                                  R10 - will be written out at the end of kernel\n" ++
  "                              ** R1-R6 are reserved **\n" ++
  "\n" ++
  "EXAMPLES:\n" ++
  " % runasm foo.sass\n" ++
  " Runs the assembly in foo.sass\n" ++
  ""

parseArgs :: Opts -> [String] -> IO Opts
parseArgs os [] = do
  when (null (oInputPath os)) $
    fatal "expected assembly input file"
  return os
parseArgs os (a:as)
  | a `elem` ["-h","--help"] = putStrLn usage >> exitSuccess
  | "-q" == a = parseArgs os{oVerbosity = -1} as
  | "-v" == a = parseArgs os{oVerbosity = 1} as
  | "-v2" == a = parseArgs os{oVerbosity = 2} as
  | "-i" == key = parseInt (\i -> os{oIterations = i})
  --
  | "-s0s" == key = parseInt (\i -> os{oSrc0s = Just (fromIntegral i)})
  | "-s1s" == key = parseInt (\i -> os{oSrc1s = Just (fromIntegral i)})
  | "-s2" == key = parseInt (\i -> os{oSrc2 = Just (fromIntegral i)})
  | "-bs" == key = parseInt (\i -> os{oBs = Just (fromIntegral i)})
  | "-tpb" == key = parseInt (\i -> os{oTpb = Just (fromIntegral i)})
  --
  | a`elem`["-pr","--preload-registers"] = parseArgs os{oPreloadRegisters = True} as
  | a`elem`["-npr","--no-preload-registers"] = parseArgs os{oPreloadRegisters = False} as
  | "-"`isPrefixOf`a = badArg "unrecognized option"
  | null (oInputPath os) = parseArgs os{oInputPath = a} as
  | otherwise = badArg "duplicate argument"
  where badArg :: String -> IO a
        badArg msg = fatal (a ++ ": " ++ msg)

        (key,eq_val) = span (/='=') a

        val = drop 1 eq_val

        parseInt :: (Int -> Opts) -> IO Opts
        parseInt f =
          case reads val of
            [(x,"")] -> parseArgs (f x) as
            _ -> badArg "malformed int"

fatal :: String -> IO a
fatal msg = do
  hPutStrLn stderr msg
  exitFailure

type RunResult = (Int,String,String)

test :: IO ()
test = runWithOpts (==ws) dft_opts{oInputPath = "trial.sass"}
  where ws :: [Word32]
        ws = []

runWithOpts :: ([Word32] -> Bool) -> Opts -> IO ()
runWithOpts referee os = do
  rrs <- runWithOptsRs os
  forM_ (zip rrs [0..]) $ \((ec,out,err),run) -> do
    -- fatal ("micro exited " ++ show ec ++ "\n" ++ out ++ err)
    putStrLn $ "============ RUN " ++ show run ++ " exited " ++ show ec
    putStr err
    putStr out
    let ws :: [String]
        ws = concatMap (drop 1 . words) (drop 2 (lines out))

        par [] = Just []
        par (w:ws) =
          case reads w of
            [(x,"")] ->
              case par ws of
                Just xs -> Just (x:xs)
                _ -> Nothing
            _ -> Nothing
    case par ws of
      Nothing -> putStrLn "output parse failed"
      Just xs -> do
        unless (referee xs) $
          putStrLn "referee failed (output mismatches expected)"
    return ()
  -- let (ecs,outs,errs) = unzip3 rrs
  unless (null rrs) $ do
    when (any (/= head rrs) (tail rrs)) $
      putStrLn "output differs"



runWithOptsRs :: Opts -> IO [RunResult]
runWithOptsRs os = do
  exe <- assembleExe os
  let args :: [String]
      args = concatMap toArg
                [
                  (oSrc0s,"-s0s")
                , (oSrc1s,"-s1s")
                , (oSrc2,"-s2")
                , (oTpb,"-tpb")
                , (oBs,"-bs")
                ]
        where toArg (f,key) =
                case f os of
                  Nothing -> []
                  Just i -> [printf "%s=0x%08X" key i]
  forM [0 .. (oIterations os - 1)] $ \i -> do
    (ec,out,err) <- readProcessWithExitCode exe [] ""
    case ec of
      ExitFailure ec -> return (ec,out,err)
      ExitSuccess -> return (0,out,err)


assembleExe :: Opts -> IO FilePath
assembleExe os = do
  when (oPreloadRegisters os) $
    fatal "register preloading not ready yet"
  z <- doesFileExist patched_exe
  when z $
    removeFile patched_exe
  --
  -- start the search for the exe location in parallel to parsing and encoding
  mv_esi <- newEmptyMVar
  forkIO (getSkeletonExeInfo mv_esi)
  --
  -- start the search through the exe while we parse and assemble the input
  inp_w128s <- assembleInput os
  let inp_bs = BS.concat $ map toByteStringU128LE inp_w128s
  BS.length inp_bs `seq` return ()
  osVerboseLn os "parse/encode done"
  --
  -- stall until exe search done
  SkeletonExeInfo isa_s isa_e bs_exe <- takeMVar mv_esi
  let free_space = isa_e - isa_s - BS.length end_seq
      nop_padding = free_space - BS.length inp_bs
  when (nop_padding < 0) $
    fatal "assembled input is too large for skeleton executable"
  when (nop_padding `mod` 16 /= 0) $
    fatal "slack space must be a multiple of 16"
  --  NOP   {Y};           /* 000FC00000000000`0000000000007918 */
  let nop_bs = toByteStringU128LE (Word128 0x000FC00000000000 0x0000000000007918)
  let new_exe =
        BS.concat [
            BS.take isa_s bs_exe
          , inp_bs
          , end_seq
          , BS.concat $ replicate (nop_padding`div`16) nop_bs -- clobber the rest with NOPs
          , BS.drop isa_e bs_exe
          ]
  BS.writeFile patched_exe new_exe
  return patched_exe


data SkeletonExeInfo =
  SkeletonExeInfo {
    seiStartOffset :: !Int
  , seiEndOffset :: !Int
  , seiBytes :: !BS.ByteString
  } deriving Show

getSkeletonExeInfo :: MVar SkeletonExeInfo -> IO ()
getSkeletonExeInfo mv = do
  bs_exe <- BS.readFile driver_skeleton
  case findIsaRanges bs_exe of
    [] -> fatal "no ISA sections found"
    (_:_:_) -> fatal "multiple ISA sections found"
    [(isa_s,isa_e)] -> do
      -- putStrLn $ "ISA offsets: " ++ show (isa_s,isa_e)
      let esi = SkeletonExeInfo isa_s isa_e bs_exe
      esi `seq` putMVar mv esi


assembleInput :: Opts -> IO [Word128]
assembleInput os = do
  fstr <- readFile (oInputPath os)
  length fstr `seq` return ()
  osVerboseLn os "*** PARSING"
  let fmtDiag = dFormatWithLines (lines fstr)
      emitWarnings ws = mapM_ (putStrLn . fmtDiag) ws
  case parseInsts (oInputPath os) fstr of
    Left err -> fatal $ dFormatWithLines (lines fstr) err
    Right (is,lbl_ix,ws) -> do
      emitWarnings ws
      osVerboseLn os "*** ENCODING"
      case runInstEncoders is of
        Left err -> fatal $ fmtDiag err
        Right (w128s,ws) -> do
          emitWarnings ws
          return w128s

-- We find the ISA range by look for the first S2R;
-- we find the end by walking in increments of 16 until we
-- have the EXIT followed by BRA loop and NOPS; that is,
-- we look for:
--       ???
--       S2R R0, SR_CTAID.X   000e220000002500'0000000000007919
--                            000e220000002500'0000000000007919
--       ... body
--       ... body
--       ... body
--       EXIT   {!5};
-- .L_1: BRA    `(.L_1) {Y};...
--       NOP   {Y};  << gone in CUDA 11
--       ???
--
-- The offsets include the end sequence as part of the space we
-- get to clobber (since we intend to replicate that higher up)
findIsaRanges :: BS.ByteString -> [(Int,Int)]
findIsaRanges bs = findStartOffsets 0 bs
  where findStartOffsets :: Int -> BS.ByteString -> [(Int,Int)]
        findStartOffsets s_off bs
          | BS.length bs < 16 = []
          | is_s2r_inst = findEndOffset (s_off + 16) (BS.drop 16 bs)
          -- | otherwise = findStartOffsets (s_off + 1) (BS.drop 1 bs)
          | otherwise = findStartOffsets (s_off + 16) (BS.drop 16 bs)
          where is_s2r_inst = lo64_matches && hi64_matches
              -- don't care about the DST register, but I do care about the SRC (I think [79:72])
              -- don't care about the alloc wr barrier [115:113]
                lo64_matches =
                    cmpMasked
                      mask_out_dst
                      (fromByteStringU64LE (BS.take 8 bs))
                      (wLo64 s2r_ctaid)
                  where mask_out_dst = complement 0x0000000000FF0000
                hi64_matches =
                    cmpMasked
                      mask_out_wrallocbid
                      (fromByteStringU64LE (BS.take 8 (BS.drop 8 bs)))
                      (wHi64 s2r_ctaid)
                  where mask_out_wrallocbid = complement 0x000E000000000000
                s2r_ctaid = Word128 0x000E220000002500 0x0000000000007919
                cmpMasked mask x y = (x .&. mask) == (y .&. mask)

                findEndOffset :: Int -> BS.ByteString -> [(Int,Int)]
                findEndOffset e_off bs
                  | BS.length bs < 16 = []
                  | BS.take (BS.length end_seq) bs == end_seq =
                    (s_off,e_off + end_seq_len):
                      findStartOffsets (e_off + end_seq_len) (BS.drop end_seq_len bs)
                  | otherwise = findEndOffset (e_off + 16) (BS.drop 16 bs)
                  where end_seq_len = BS.length end_seq

findStart :: Word32 -> BS.ByteString -> Maybe Int
findStart find = loop 0
  where loop :: Int -> BS.ByteString -> Maybe Int
        loop off bs
          | BS.length bs < 4 = Nothing
          | find == w32 = Just off
          | otherwise = loop (off + 1) (BS.tail bs)
          where (pfx,sfx) = BS.splitAt 4 bs
                w32 = fromByteStringU32LE pfx

end_seq :: BS.ByteString
end_seq = foldl' BS.append BS.empty (map toByteStringU64LE end_ws)
  where end_ws =
          [
            0x000000000000794D, 0x000FEA0003800000 --       EXIT   {!5};
          , 0xFFFFFFF000007947, 0x000FC0000383FFFF -- .L_1: BRA    `(.L_1) {Y};
--          , 0x0000000000007918, 0x000FC00000000000 --       NOP   {Y};

          ]


{-
write_seq :: BS.ByteString
write_seq = foldl' BS.append BS.empty (map toByteStringU64LE end_ws)
  where end_ws =
          [
            000E220000002500, 0000000000007919 -- S2R       R0, SR_CTAID.X  {!1,+1.W};
          , 000FE200078E00FF, 00000004FF037424 -- IMAD.MOV  R3,     RZ,     RZ,     0x4              {!1};
          , 0xFFFFFFF000007947, 0x000FC0000383FFFF -- .L_1: BRA    `(.L_1) {Y};
--          , 0x0000000000007918, 0x000FC00000000000 --       NOP   {Y};
          ]

-}