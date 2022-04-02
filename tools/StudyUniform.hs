import Control.Monad
import Data.Char
import Data.List
import System.FilePath
import System.Directory
import Text.Printf

study :: IO ()
study = do
  putStrLn $ printf "%-12s %8s | %-12s %8s" "UOP" "" "VOP" ""
  forM_ op_list $ \(uni,vec) -> do
    uops <- countOps uni
    vops <- countOps vec
    let pct = (100.0 * fromIntegral uops / fromIntegral (uops + vops)) :: Double
    putStrLn $ printf "%-12s %8d | %-12s %8d |  %5.2f %% uniform" uni uops vec vops pct

countOps :: String -> IO Int
countOps op = do
  let fp = "examples/sm_86/ops/" ++ op ++ ".sass"
  -- <- doesFileExist ()
  length . lines <$> readFile fp

op_list :: [(String,String)]
op_list =
   [
--      op "BRXU" "BRX"
      op "S2UR"   "S2R"
    , op "UFLO"   "FLO"
    , op "UIADD3" "IADD3"
    , op "UIMAD"  "IMAD"
    , op "UISETP" "ISETP"
    , op "ULDC"   "LDC"
    , op "ULEA"   "LEA"
    , op "ULOP3"  "LOP3"
    , op "UMOV"   "MOV"
    , op "UPOPC"  "POPC"
    , op "UPRMT"  "PRMT"
    , op "USEL"   "SEL"
    , op "USGXT"  "SGXT"
    , op "USHF"   "SHF"
    , op "VOTEU"  "VOTE"
    ]
  where op = (,)