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


opts :: Opts
opts = dft_opts_75


-- 000E280000005800`0004200002097984:        LDS.U R9, [R2.X4+0x420] {!4,+1.W} ;    // examples/sm_75/samples\bitonic.sass:3554
-- 000E220000009A00`0008000000027984:        LDS.U.64 R2, [R0.X8+0x800] {!1,+1.W} ; // examples/sm_75/samples\bitonic.sass:3652
--

-- e.g. 000EE800001EE900`0000000006068381
sString :: String -> IO [Sample]
sString = return . (\s -> [s]) . sStringPure
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
          _ -> fail ("sFile ("++fp++"): error on line " ++ show lno)
  mapM parseLine flns


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

sNorm :: IO [Sample] -> IO [Sample]
sNorm io_ss = io_ss *~> [((24,8),0),((32,8),0),((64,8),0)]