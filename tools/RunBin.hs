module RunBin where

import NVT.Bits
import NVT.Diagnostic
import NVT.Encoders.InstEncoder
import NVT.IR
import NVT.Loc
import NVT.Parsers.InstParser


import Control.Monad
import Data.List
import Data.Bits
import Data.Word
import System.Environment
import System.Exit
import System.IO
import System.Directory
import System.Process
import qualified Data.ByteString as BS

main :: IO ()
main = getArgs >>= run

data Opts =
  Opts {
    oVerbosity :: !Int
  , oSaveExe :: !Bool
  , oPath :: !FilePath
  } deriving Show
dft_opts :: Opts
dft_opts = Opts 0 False ""

run :: [String] -> IO ()
run as = parseArgs dft_opts as >>= runWithOpts

vERSION :: String
vERSION = "0.0.0"

usage :: String
usage =
  "Run Assembly (" ++ vERSION ++ ")\n" ++
  "\n" ++
  "usage: runasm.exe [OPTIONS]  <INPUT>\n" ++
  "where OPTIONS are:\n" ++
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

fatal :: String -> IO a
fatal msg = do
  hPutStrLn stderr msg
  exitFailure

driver_skeleton :: FilePath
-- driver_skeleton = "micro_skeleton.exe"
driver_skeleton = "micro_skeleton.cubin"

runWithOpts :: Opts -> IO ()
runWithOpts os = do
  exe <- assembleExe os
  (ec,out,err) <- readProcessWithExitCode exe [] ""
  case ec of
    ExitFailure ec -> fatal ("micro exited " ++ show ec ++ "\n" ++ out ++ err)
    ExitSuccess -> return ()


assembleExe :: Opts -> IO FilePath
assembleExe os = do
  z <- doesFileExist patched_exe
  when z $
    removeFile patched_exe
  inp_w128s <- assembleInput os :: IO [Word128]
  let inp_bs = BS.concat $ map toByteStringU128LE inp_w128s
  bs <- BS.readFile driver_skeleton
  (isa_s,isa_e) <- findIsa bs
  putStrLn $ "ISA offsets: " ++ show (isa_s,isa_e)
  let free_space = isa_e - isa_s - BS.length end_seq
      nop_padding = free_space - BS.length inp_bs
  when (nop_padding < 0) $
    fail "assembled ISA too large for executable"
  when (nop_padding `mod` 16 /= 0) $
    fail "slack space must be a multiple of 16"
  --  NOP   {Y};           /* 000FC00000000000`0000000000007918 */
  let nop_bs = toByteStringU128LE (Word128 0x000FC00000000000 0x0000000000007918)
  let new_isa =
        BS.concat [
            BS.take isa_s bs
          , inp_bs
          , end_seq
          , BS.concat $ replicate (nop_padding`div`16) nop_bs -- clobber the rest with NOPs
          , BS.drop isa_e bs
          ]
  BS.writeFile patched_exe new_isa
  return patched_exe

patched_exe :: FilePath
-- patched_exe = "patched.exe"
patched_exe = "patched.cubin"


assembleInput :: Opts -> IO [Word128]
assembleInput os = do
  fstr <- readFile (oPath os)
  length fstr `seq` return ()
  putStrLn "*** PARSING"
  let fmtDiag = dFormatWithLines (lines fstr)
      emitWarnings ws = mapM_ (putStrLn . fmtDiag) ws
  case parseInstsUnresolved [] 0 (oPath os) 1 fstr of
    Left err -> fatal $ dFormatWithLines (lines fstr) err
    Right ((uis,lbl_ix),ws) -> do
      emitWarnings ws
      case uis lbl_ix of
        Left err -> fatal $ fmtDiag err
        Right is -> do
          putStrLn "*** ENCODING"
          case runInstEncoders is of
            Left err -> fatal $ fmtDiag err
            Right (w128s,ws) -> do
              emitWarnings ws
              return w128s


findIsa :: BS.ByteString -> IO (Int,Int)
findIsa bs = findStart 0 bs
  where findStart :: Int -> BS.ByteString -> IO (Int,Int)
        findStart s_off bs
          | BS.null bs = fail "unable to find start of ISA"
          | is_s2r_inst = findEnd (s_off+16) (BS.drop 16 bs)
          | otherwise = findStart (s_off+1) (BS.drop 1 bs)
          where s2r_mask_out_dst = complement 0x0000000000FF0000
                is_s2r_inst = fromByteStringU64LE (BS.take 8 bs) .&. s2r_mask_out_dst == 0x0000000000007919
                findEnd e_off bs
                  | BS.null bs = fail "unable to find end of ISA"
                  | BS.take (BS.length end_seq) bs == end_seq = return (s_off,e_off+BS.length end_seq)
                  | otherwise = findEnd (e_off + 16) (BS.drop 16 bs)

end_seq :: BS.ByteString
end_seq = foldl' BS.append BS.empty (map toByteStringU64LE end_ws)
  where end_ws =
          [
            0x000000000000794D, 0x000FEA0003800000 --       EXIT   {!5};
          , 0xFFFFFFF000007947, 0x000FC0000383FFFF -- .L_1: BRA    `(.L_1) {Y};
          , 0x0000000000007918, 0x000FC00000000000 --       NOP   {Y};
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