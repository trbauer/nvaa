import Control.Monad
import Data.Char
import Data.List
import System.FilePath
import System.Directory
import Text.Printf

study90 = study "sm_90"
study86 = study "sm_86"
study80 = study "sm_80"

study :: String -> IO ()
study sm_ver = do
  putStrLn $ printf "%-12s %8s | %-12s %8s" "UOP" "" "VOP" ""
  forM_ op_list $ \(uni,vec) -> do
    uops <- countOps sm_ver uni
    vops <- countOps sm_ver vec
    let pct = (100.0 * fromIntegral uops / fromIntegral (uops + vops)) :: Double
    putStrLn $ printf "%-12s %8d | %-12s %8d |  %5.2f %% uniform" uni uops vec vops pct

countOps :: String -> String -> IO Int
countOps sm_ver op = do
--  let fp = "examples/sm_86/ops/" ++ op ++ ".sass"
  let fp = "examples/" ++ sm_ver ++ "/ops/" ++ op ++ ".sass"
  z <- doesFileExist fp
  if z then length . lines <$> readFile fp
    else return 0

op_list :: [(String,String)]
op_list =
   [
--      op "BRXU" "BRX"
      op "S2UR"   "S2R"
    , op "UBREV"  "BREV"
    , op "UFLO"   "FLO"
    , op "UIADD3" "IADD3"
    , op "UIMAD"  "IMAD"
    , op "UISETP" "ISETP"
    , op "ULDC"   "LDC"
    , op "ULEA"   "LEA"
    , op "ULOP3"  "LOP3"
    , op "UMOV"   "MOV"
    , op "UP2UR"  "P2R"
    , op "UPOPC"  "POPC"
    , op "UPLOP3" "PLOP3"
    , op "UPRMT"  "PRMT"
    , op "USEL"   "SEL"
    , op "USGXT"  "SGXT"
    , op "USHF"   "SHF"
    , op "VOTEU"  "VOTE"
    ]
  where op = (,)