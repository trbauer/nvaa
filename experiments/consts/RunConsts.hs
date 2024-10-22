import Data.List
import Data.Char
import Data.Int
import System.Process
import System.Exit
import System.Environment
import Text.Printf
-- import qualified Data.ByteString as BS



-- main :: IO ()
-- main = getArgs >>= run

-- run :: [String] -> IO ()
-- run as = do

test :: Int -> IO ()
test n = do
  let scr =
--        "let OUT = 0:w\n" ++
--        "let OUTT = 0:w\n" ++
        "#0`consts.cl[-DK=" ++ show n ++ "]`walkConsts<1>(0:w,0:wp,0,cyc(" ++ show (n-1) ++ ",0):r)\n"
  putStrLn scr
  ou <- readProcess "cls64.exe" ["-e",scr] ""
  putStrLn ou
  return ()



