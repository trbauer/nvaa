module MakeOpsTable where

import qualified NVT.CUDASDK as D

import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Printf

import qualified Data.Map.Strict as DM

eXAMPLES_DIR :: FilePath
eXAMPLES_DIR = "examples"

-- main :: IO ()
-- main = getArgs >>= run

data MOTOpts =
  MOTOpts {
    motoVerbosity :: !Int
  } deriving (Show,Eq)

-- run :: [String] -> IO ()
-- run as = runWithOpts (MOTOpts 0)

fatal :: String -> IO a
fatal = die

type SmName = String
type OpName = String
-- type SmOpsTable = [(SmName,[(OpName,Int,Double)])]



type SmOpsTable = [SmsOpsEntry]
type SmsOpsEntry = ((SmName,OpName),(Int,Double))

computeData :: IO ()
computeData = do
    sm_dirs <- D.getSubPaths eXAMPLES_DIR
    -- str <- fmtTbl . transposeTable . concat <$> mapM processSm sm_dirs
    -- putStrLn str
    xs <- concat <$> mapM processSm sm_dirs
    mapM_ print xs
    writeFile "ops-by-sm.dat" (show xs)
  where
        processSm :: FilePath -> IO [SmsOpsEntry]
        processSm sm_fp = do
          let ops_dir = sm_fp ++ "/ops"
          z <- doesDirectoryExist ops_dir
          if not z then putStrLn (ops_dir ++ ": directory doesn't exist") >> return []
            else do
              op_fps0 <- D.getSubPaths ops_dir
              op_fps <- filterM doesFileExist op_fps0
              op_ns <- mapM processSmOp op_fps -- [(String,Int)]
              let n_ops :: Double
                  n_ops = fromIntegral (sum (map snd op_ns))

                  toPct :: Int -> Double
                  toPct n = fromIntegral n / n_ops

                  sm_name :: String
                  sm_name = takeFileName $ sm_fp

                  row :: [SmsOpsEntry]
                  row = map (\(op,nops) -> ((op,sm_name),(nops,toPct nops))) op_ns

              return row

        processSmOp :: FilePath -> IO (OpName,Int)
        processSmOp op_fp = do
          -- n_ops <- length . lines <$> readFile op_fp
          n_ops <- read . head . words <$> readProcess "wc.exe" [op_fp] ""
          return (dropExtension . takeFileName $ op_fp, n_ops)

--------------------
--   SM5.0    SM7.5 ...
--   IADD
--            IADD3
--
--          SM...
-- IADD
-- IADD3
emitTableFromData :: IO ()
emitTableFromData = do
  tbl <- read <$> readFile "ops-by-sm.dat" :: IO SmOpsTable
  putStrLn $ fmtTbl tbl
  return ()

fmtTbl :: SmOpsTable -> String
fmtTbl tbl =
    header ++
    concatMap fmtOp ops
  where sms :: [SmName]
        sms = nub . sort . map (snd . fst) $ tbl

        ops :: [OpName]
        ops = nub . sort . map (fst . fst) $ tbl

        header :: String
        header =
          printf "%-32s" "OP" ++
            intercalate "  " (map (printf "%-12s") sms) ++ "\n"

        fmtOp :: OpName -> String
        fmtOp op_nm =
            printf "%-32s" (op_nm ++ ": ") ++
              intercalate "  " (map fmtSm sms) ++ "\n"
          where fmtSm :: SmName -> String
                fmtSm sm_nm = printf "%-12s" $
                    case filter ((== (op_nm,sm_nm)) . fst) tbl of
                      [] -> ""
                      [(_,(n,pct))]
                          | n == 0 -> ""
                          | otherwise -> printf "%7.4f %%" (100.0 * pct)
