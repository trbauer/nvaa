module RunBin where

import NVT.Bits
import NVT.Diagnostic
import NVT.Encoders.InstEncoder
import NVT.IR
import NVT.Loc
import NVT.Parsers.InstParser


import Control.Monad
import Control.Concurrent(forkIO)
import Control.Concurrent.MVar
import Data.List
import Data.Bits
import Data.Word
import Debug.Trace
import System.Environment
import System.Exit
import System.IO
import System.Directory
import System.Process
import Text.Printf
import qualified Data.ByteString as BS

driver_skeleton :: FilePath
driver_skeleton = "itester/itester.exe"
-- driver_skeleton = "micro_skeleton.cubin"

patched_exe :: FilePath
patched_exe = "itester/itester_patched.exe"
-- patched_exe = "patched.cubin"

main :: IO ()
main = getArgs >>= run

data Opts =
  Opts {
    oArgs :: ![String]
  , oIterations :: !Int
  , oPath :: !FilePath
  , oSaveExe :: !Bool
  , oVerbosity :: !Int
  } deriving Show
osVerboseLn :: Opts -> String -> IO ()
-- osVerboseLn os = when (oVerbosity os > 0) . putStrLn
osVerboseLn os s = do
  when (oVerbosity os > 0) $ do
    length s `seq` putStrLn s

dft_opts :: Opts
dft_opts =
  Opts {
    oArgs = []
  , oIterations = 1
  , oPath = ""
  , oSaveExe = False
  , oVerbosity = 0
  }

run :: [String] -> IO ()
run as = parseArgs dft_opts as >>= runWithOpts (const True)


vERSION :: String
vERSION = "1.0.0"

usage :: String
usage =
  "Run Assembly (" ++ vERSION ++ ")\n" ++
  "\n" ++
  "usage: runasm.exe [OPTIONS]  <INPUT>\n" ++
  "where OPTIONS are:\n" ++
  "  -a=ARG           passes an argument \n" ++
  "  -i=INT           iterations (default " ++ show (oIterations dft_opts) ++ ")\n" ++
  "  -v/-v2/-q        verbose/debug/quiet\n" ++
  "\n" ++
  "EXAMPLES:\n" ++
  " % runasm foo.sass\n" ++
  " Runs the assembly in foo.sass\n" ++
  ""

parseArgs :: Opts -> [String] -> IO Opts
parseArgs os [] = do
  when (null (oPath os)) $
    fatal "expected assembly input file"
  return os
parseArgs os (a:as)
  | a `elem` ["-h","--help"] = do
    putStrLn usage
    exitSuccess
  | "-a" == key = parseArgs os{oArgs = oArgs os ++ [val]} as
  | "-i" == key = parseInt (\i -> os{oIterations = i})
  | "-q" == a = parseArgs os{oVerbosity = -1} as
  | "-v" == a = parseArgs os{oVerbosity = 1} as
  | "-v2" == a = parseArgs os{oVerbosity = 2} as
  | "-"`isPrefixOf`a = badArg "unrecognized option"
  | null (oPath os) = parseArgs os{oPath=a} as
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
test = runWithOpts (==ws) dft_opts{oPath = "trial.sass"}
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
  when (oIterations os == 0) $
    putStrLn $ exe ++ ": created patched exe"
  forM [0 .. (oIterations os - 1)] $ \i -> do
    (ec,out,err) <- readProcessWithExitCode exe (oArgs os) ""
    case ec of
      ExitFailure ec -> return (ec,out,err)
      ExitSuccess -> return (0,out,err)



data SkeletonExeInfo =
  SkeletonExeInfo !Int !Int !BS.ByteString



assembleExe :: Opts -> IO FilePath
assembleExe os = do
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
    fatal "assembled ISA too large for executable"
  when (nop_padding `mod` 16 /= 0) $
    fatal "slack space must be a multiple of 16"
  --  NOP   {Y};           /* 000FC00000000000`0000000000007918 */
  let nop_bs = toByteStringU128LE (Word128 0x000FC00000000000 0x0000000000007918)
  let new_isa =
        BS.concat [
            BS.take isa_s bs_exe
            -- FIXME: we should insert a hard stall here to flush the pipeline
          , inp_bs
          , end_seq
          , BS.concat $ replicate (nop_padding`div`16) nop_bs -- clobber the rest with NOPs
          , BS.drop isa_e bs_exe
          ]
  BS.writeFile patched_exe new_isa
  return patched_exe


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
  fstr <- readFile (oPath os)
  length fstr `seq` return ()
  osVerboseLn os "*** PARSING"
  let fmtDiag = dFormatWithLines (lines fstr)
      emitWarnings ws = mapM_ (putStrLn . fmtDiag) ws
  case parseInstsUnresolved [] 0 (oPath os) 1 fstr of
    Left err -> fatal $ dFormatWithLines (lines fstr) err
    Right ((uis,lbl_ix),ws) -> do
      emitWarnings ws
      case uis lbl_ix of
        Left err -> fatal $ fmtDiag err
        Right is -> do
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
--       ... body
--       ... body
--       ... body
--       EXIT   {!5};
-- .L_1: BRA    `(.L_1) {Y};...
--       ???
--
-- The offsets include the end sequence as part of the space we
-- get to clobber (since we intend to replicate that higher up)
findIsaRanges :: BS.ByteString -> [(Int,Int)]
findIsaRanges = findStartOffsets 0
  where findStartOffsets :: Int -> BS.ByteString -> [(Int,Int)]
        findStartOffsets s_off bs
          | BS.length bs < 16 = []
          | is_s2r_inst = findEndOffset (s_off+16) (BS.drop 16 bs)
          | otherwise = findStartOffsets (s_off+1) (BS.drop 1 bs)
          where is_s2r_inst = lo64_matches && hi64_matches
                  where (lo8B,bs_tmp) = BS.splitAt 8 bs
                        hi8B = BS.take 8 bs_tmp

                        lo64_matches :: Bool
                        lo64_matches =
                            cmpMasked
                              mask_out_dst
                              (fromByteStringU64LE lo8B)
                              (wLo64 s2r_ctaid)
                                -- don't care about the DST register, but I do care about the SRC (I think [79:72])
                          where mask_out_dst = complement 0x0000000000FF0000

                        hi64_matches :: Bool
                        hi64_matches =
                            cmpMasked
                              mask_out_depinfo
                              (fromByteStringU64LE hi8B)
                              (wHi64 s2r_ctaid)
                                -- don't care about any alloc wr barrier [112:110]
                                -- don't care about any alloc rd barrier [115:113]
                                -- or the stall count ..., or yield, ..., or wait mask
                                -- we just cop-out and mask the entire top bits [127:108]
                          where mask_out_depinfo = complement 0xFFFFFF0000000000

                        s2r_ctaid :: Word128
                        s2r_ctaid = Word128 0x000e220000002500 0x0000000000007919

                        cmpMasked :: Word64 -> Word64 -> Word64 -> Bool
                        cmpMasked mask x y =
                          (x .&. mask) == (y .&. mask)

                findEndOffset :: Int -> BS.ByteString -> [(Int,Int)]
                findEndOffset e_off bs
                  | BS.length bs < 16 = []
                  | BS.take (BS.length end_seq) bs == end_seq =
                    (s_off,e_off+end_seq_len):
                      findStartOffsets (e_off+end_seq_len) (BS.drop end_seq_len bs)
                  | otherwise = findEndOffset (e_off + 16) (BS.drop 16 bs)
                  where end_seq_len = BS.length end_seq


end_seq :: BS.ByteString
end_seq = foldl' BS.append BS.empty (map toByteStringU64LE end_ws)
  where end_ws =
          [
            0x000000000000794D, 0x000FEA0003800000 --       EXIT   {!5};
          , 0xFFFFFFF000007947, 0x000FC0000383FFFF -- .L_1: BRA    `(.L_1) {Y};
         -- , 0x0000000000007918, 0x000FC00000000000 --       NOP   {Y};
          ]


{-
findInstSeqOffsets :: [Word64] -> BS.ByteString -> [Int]
findInstSeqOffsets ws bs = findAll 0 bs
  where
        findAll :: Int -> BS.ByteString -> [Int]
        findAll off bs
          | BS.null bs = []
          | ss `BS.isPrefixOf`bs =
            off:findAll (off+ss_len) (BS.drop ss_len bs)
          | otherwise = findAll (off+1) (BS.drop 1 bs)
-}