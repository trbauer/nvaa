module Sample where

import NVT.Bits
import NVT.CUDASDK
import NVT.Opts

import Control.Monad
import Data.Char
import Data.List
import Data.Word


data Sample = Sample {sBits :: !Word128}
  deriving (Eq,Ord,Show)

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


dft_analysis_opts :: Opts
-- dft_opts = dft_opts_75
-- dft_analysis_opts = dft_opts_80
dft_analysis_opts = dft_opts_86

-- 000E280000005800`0004200002097984:        LDS.U R9, [R2.X4+0x420] {!4,+1.W} ;    // examples/sm_75/samples\bitonic.sass:3554
-- 000E220000009A00`0008000000027984:        LDS.U.64 R2, [R0.X8+0x800] {!1,+1.W} ; // examples/sm_75/samples\bitonic.sass:3652
--

-- e.g. 000EE800001EE900`0000000006068381
sString :: String -> IO [Sample]
sString = return . (\s -> [s]) . sStringPure
sS :: String -> IO [Sample]
sS = sString
sStrings :: [String] -> IO [Sample]
sStrings = return . map sStringPure
sStringPure :: String -> Sample
sStringPure str =
  case reads ("0x" ++ str) of
    [(s,"")] -> Sample s
    _ -> error ("sFromBits: syntax error: " ++ show str)
sAllOps :: String -> IO [Sample]
sAllOps = sAllOpsK (-1)
sAllOpsK :: Int -> String -> IO [Sample]
sAllOpsK k opname =
  sFileK k ("examples/" ++ oArch dft_analysis_opts ++ "/ops/" ++ map toUpper opname++".sass")
sDir :: FilePath -> IO [Sample]
sDir dir = do
  fs <- getSubPaths dir
  concat <$> mapM sFile fs
sFile :: FilePath -> IO [Sample]
sFile = sFileK (-1)

sFileK :: Int -> FilePath -> IO [Sample]
sFileK k fp = do
  -- e.g. 000EE200001EED00`00001000020C7381:        LDG.E.128.SYS R12, [R2+0x10] {!1,+4.W} ; // examples/sm_75/samples\bodysystemcuda.sass:20925
  let maybeTake = if k < 0 then id else take k
  flns <- maybeTake . zip [1..] . lines <$> readFile fp
  let parseLine :: (Int,String) -> IO [Sample]
      parseLine (lno,lnstr) =
        case dropWhile isSpace lnstr of
          "" -> return []
          '/':'/':_ -> return []
          str ->
            case reads ("0x"++lnstr) of
              [(w128,_)] -> return [Sample w128]
              _ -> fail ("sFile "++show fp++": error on line " ++ show lno)
  concat <$> mapM parseLine flns


(+++) :: IO [Sample] -> IO [Sample] -> IO [Sample]
(+++) io_ss_a io_ss_b = liftM2 (++) io_ss_a io_ss_b
infixl 5 +++
(~>) :: IO [Sample] -> (Field,Word64) -> IO [Sample]
(~>) io_s (f,val) = io_s *~> [(f,val)]
(*~>) :: IO [Sample] -> [(Field,Word64)] -> IO [Sample]
(*~>) io_ss fvs = do
  ss <- io_ss
  let acc :: Sample -> (Field,Word64) -> Sample
      acc s (f,v) = s{sBits = putField128 (fOff f) (fLen f) v (sBits s)}
  return $ map (\s0 -> foldl' acc s0 fvs) ss

infixl 2 ~>
infixl 3 .=

(.=) :: Field -> Word64 -> (Field,Word64)
(.=) = (,)

sNorm :: IO [Sample] -> IO [Sample]
sNorm io_ss = io_ss *~> [((24,8),0),((32,8),0),((64,8),0)]
