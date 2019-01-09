module LUT where

import BE

import Control.Monad
import Data.Array
import Data.Bits
import Data.Char
import Data.List
import Data.Ord
import Data.Word
import Debug.Trace
import Text.Printf

import qualified Data.Map.Strict as DM
-- import qualified Data.Set.Strict as DS


beNegate :: BE -> BE
beNegate be =
  case be of
    BEVarA -> BENeg BEVarA
    BEVarB -> BENeg BEVarB
    BEVarC -> BENeg BEVarC
    BECon0 -> BECon1
    BECon1 -> BECon0
    BENeg be -> be
    BEBin BEOr l r -> BEBin BEAnd (beNegate l) (beNegate r)
    BEBin BEAnd l r -> BEBin BEOr (beNegate l) (beNegate r)
    _ -> be -- DeMorgan's Law doesn't hold for Xor

-- ~(e) is e
beAbs :: BE -> BE
beAbs (BENeg be) = be
beAbs be = be
beAtomOrder :: BE -> Int
beAtomOrder BEVarA = 0
beAtomOrder BEVarB = 1
beAtomOrder BEVarC = 2
beAtomOrder (BENeg be) = beAtomOrder be
beIsAtom :: BE -> Bool
beIsAtom BEVarA = True
beIsAtom BEVarB = True
beIsAtom BEVarC = True
beIsAtom (BENeg be) = beIsAtom be
beIsAtom _ = False
beOrd :: BE -> Int
beOrd be
  | beIsAtom be = beAtomOrder be
  | otherwise =
    case be of
      BEBin _ be_l _ -> beOrd be_l

-- returns the count of binary operators
beSize :: BE -> Int
beSize (BEBin _ x y) = beSize x + beSize y + 1
beSize (BENeg x) = beSize x
beSize _ = 0

beAllSized :: Int -> [BE]
beAllSized k = lookupRow True k

be_table_init_size :: Int
be_table_init_size = 10
be_table_init :: Array Int [BE]
be_table_init = mkTable False be_table_init_size
be_table_norm_init :: Array Int [BE]
be_table_norm_init = mkTable True be_table_init_size

mkTable :: Bool -> Int -> Array Int [BE]
mkTable norm k0 = listArray (0,k0) $ map mkElem [0 .. k0]
  where mkElem :: Int -> [BE]
        mkElem 0 = [BECon0,BECon1] ++ base_atoms
        mkElem 1 = prune 1 $ generateBEBins base_atoms base_atoms
        mkElem k = prune k $ concatMap addLeftSized (splits k)
            -- only favor left-heavy sincy ops are commutative
          where addLeftSized l_size =
                  generateBEBins
                    (lookupRow norm l_size)
                    (lookupRow norm (k - 1 - l_size))

        prune k
          | norm = filter ((==k) . beSize) . nubOrd . map beNormalForm
          | otherwise = id

        base_atoms :: [BE]
        base_atoms = sortBy beCompare (vs ++ map BENeg vs)
          where vs = [BEVarA,BEVarB,BEVarC]

nubOrd :: Ord a => [a] -> [a]
nubOrd = map fst . DM.toList . DM.fromList . map (\a -> (a,()))

splits :: Int -> [Int]
-- splits k = [k`div`2 .. k - 1]
-- splits :: Int -> [Int]
-- splits k = [0 .. ((k + 1)`div`2)] -- reverse [((k + 1)`div`2) .. k - 1]
splits k = [0 .. k - 1]

lookupRow :: Bool -> Int -> [BE]
lookupRow False j = if j <= be_table_init_size then be_table_init ! j else mkTable False j ! j
lookupRow  True j = -- trace ("lookupRow(" ++ show j ++ ")") $
  if j <= be_table_init_size then be_table_norm_init ! j else mkTable True j ! j

generateBEBins :: [BE] -> [BE] -> [BE]
generateBEBins ls rs =
  [BEBin op l r |
    op <-[BEAnd,BEOr,BEXor], l<-ls, r<-rs,
    beCompare l r == LT]

listBesSized :: Int -> IO ()
listBesSized k = do
  forM_ (beAllSized k) $ \be ->
    putStrLn $ printf "%-16s 0x%02X" (beFormat be ++ ":") (beEvalWithDefaults be)
listBesCountSized :: Int -> IO  ()
listBesCountSized k = do
  forM_ [0..k] $ \k -> do
--    let with_pruning = if p then " (with pruning)" else ""
    putStrLn $
      "expressions of size " ++ show k ++ ": " ++
      printf "%-8d" (length (beAllSized k)) -- ++ with_pruning
listTable :: IO  ()
listTable = body
  where body = do
          let bes = map beAllSized [0 .. 5]
          length bes `seq` return ()
          putStrLn "evaluated"
          mapM_ (listVal bes) [0..]

        listVal :: [[BE]] -> Word8 -> IO ()
        listVal bess val = do
          putStr $ printf "0x%02x" val ++ ": "
          let trySizes :: [[BE]] -> IO ()
              trySizes [] = putStrLn " NO MATCHES"
              trySizes (bes:bess) = do
                let bes_matching = filter ((==val) . beEval tVALS3) bes
                if null bes_matching then trySizes bess
                  else do
                    let top_k = 1000
                    putStrLn ""
                    putStr $ concatMap (\be -> "  " ++ beFormat be ++ "\n") (take top_k bes_matching)
                    when (length bes_matching > top_k) $
                      putStrLn $ "  ... " ++ show (length bes_matching - top_k) ++ " more"
          trySizes bess


mkFormatterTableCPP :: IO ()
mkFormatterTableCPP = do
  writeFile "out.hpp" ""
  let mkSep 0 = "  "
      mkSep _ = ", "
  appendFile "out.hpp" $
    "static const char *FMT_TABLE[256] =\n" ++
    "    {\n" ++
    concatMap (\i ->  "    " ++ mkSep i ++ fmtHppEntry i) [0..255] ++
    "    };"

fmtHppEntry :: Word8 -> String
fmtHppEntry val =
  case val `lookup` shortest_matching of
    Just bes -> printf "%-24s" (show (beFormat (head bes))) ++ " // " ++ printf "0x%02X" val ++ "\n"


mkFormatterTableHs :: IO ()
mkFormatterTableHs = do
  writeFile "out.hs" ""
  let mkSep 0 = "  "
      mkSep _ = ", "
  appendFile "out.hs" $
    "LOP3_TABLE =\n" ++
    "  [\n" ++
    concatMap (\i ->  "  " ++ mkSep i ++ fmtHsEntry i) [0..255] ++
    "  ];"
fmtHsEntry :: Word8 -> String
fmtHsEntry val =
  case val `lookup` shortest_matching of
    Just bes -> printf "%-24s" (show (beFormat (head bes))) ++ " -- " ++ printf "lop3.0x%02X" val ++ "\n"



toBinW8 :: Word8 -> String
toBinW8 val = map (\ix -> if testBit val ix then '1' else '0') [7,6,5,4,3,2,1,0]

shortest_matching :: [(Word8,[BE])]
shortest_matching = map mkElem [0..255]
  where mkElem :: Word8 -> (Word8,[BE])
        mkElem val = trySize 0
          where trySize k
                  | null bes = trySize (k+1)
                  | otherwise = (val,bes)
                  where bes = filter ((==val) . beEval tVALS3) (beAllSized k)

decodeCode :: Word8 -> String
decodeCode val =
  case val `lookup` shortest_matching of
    Nothing -> "???"
    Just bes -> beFormat (head bes)



beCheckNormalForms :: IO ()
beCheckNormalForms = mapM_ checkSize [0..]
  where checkSize :: Int -> IO ()
        checkSize k = do
          putStrLn $ "*********** checking size " ++ show k
          mapM_ checkBE (lookupRow False k)
        checkBE :: BE -> IO ()
        checkBE be
          | beEvalWithDefaults be == beEvalWithDefaults (beNormalForm be) = return ()
          | otherwise = do
            putStrLn $ "ERROR: " ++ beFormat be ++ " reduces to " ++ beFormat (beNormalForm be)
            fail "stopping"


findMatchingExpressionsFor :: Word8 -> IO ()
findMatchingExpressionsFor val = mapM_ checkSize [0 ..]
  where checkSize :: Int -> IO ()
        checkSize k = do
          putStrLn $ "*********** checking size " ++ show k
          mapM_ (checkBE k) (lookupRow True k)
        checkBE :: Int -> BE -> IO ()
        checkBE k be
          | beEvalWithDefaults be == val = putStrLn $ beFormat (beNormalForm be)
          | otherwise = return ()


tVALS3 :: (Word8,Word8,Word8)
tVALS3 = (0xF0,0xCC,0xAA) -- NVidia LOP3
-- tVALS3 = (0xAA,0xCC,0xF0) -- With flipped Src0 and Src2

tVALS2 ::(Word8,Word8,Word8)
tVALS2 = (0xC,0xA,0)

beEvalWithDefaults :: BE -> Word8
beEvalWithDefaults = beEval tVALS3


