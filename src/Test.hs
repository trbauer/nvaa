module Test where

import NVT.Opts
import qualified NVT.Driver as D

import Control.Monad
import System.Directory

tests :: IO ()
tests = do
  runTests ts_all
  return ()

help :: IO ()
help = do
  putStr $
    "% tests                   to run all\n" ++
    "% runTests ts_comp        to run compile\n" ++
    "% runTests ts_coll        to run -c tests\n" ++
    ""

runTests :: [(String,IO ())] -> IO ()
runTests ts = do
  forM_ ts $ \(tnm,t) -> do
    putStrLn $ "\n********** " ++ tnm
    t
    putStrLn "===> ENTER TO CONTINUE <===="
    getLine

ts_all :: [(String,IO ())]
ts_all =
  ts_comp ++
  ts_separator ++
  ts_coll

ts_separator = [("*****",return ())]

ts_comp =
    [
      t "test_compile1" test_compile1
    , t "test_compile2" test_compile2
    , t "test_compile_lines" test_compile_lines
    , t "test_compile_save_ptx1" test_compile_save_ptx1
    , t "test_compile_save_ptx2" test_compile_save_ptx2
    , t "test_compile_save_ptx3" test_compile_save_ptx3
    ]
  where t = (,)

tCreatesOutputFile :: FilePath -> IO () -> IO ()
tCreatesOutputFile fp t = do
  z <- doesFileExist fp
  when z $ removeFile fp
  t
  z <- doesFileExist fp
  unless z $
    fatal $ "ERROR: ==================> expected output file missing\n" ++ fp
  when z $ removeFile fp



-- just raw sass
test_compile1 :: IO ()
test_compile1 = D.run ["micros\\addrspaces.cu","-a=sm_80"]
test_compile2 :: IO ()
test_compile2 =
  tCreatesOutputFile "test.sass" $
    D.run ["micros\\addrspaces.cu","-a=sm_80","-o=test.sass"]
test_compile_lines :: IO ()
test_compile_lines = D.run ["micros\\addrspaces.cu","-a=sm_80","-lines"]
test_compile_save_ptx1 :: IO ()
test_compile_save_ptx1 = D.run ["micros\\addrspaces.cu","-a=sm_80","-lines","-ptx"]
test_compile_save_ptx2 :: IO ()
test_compile_save_ptx2 = D.run ["micros\\addrspaces.cu","-a=sm_80",         "-ptx"]
test_compile_save_ptx3 :: IO ()
test_compile_save_ptx3 =
  tCreatesOutputFile "test.ptx" $
    D.run ["micros\\addrspaces.cu","-a=sm_80",         "--save-ptx-to=test.ptx"]


ts_coll =
    [
      t "test_collate_s1" test_collate_s1
    , t "test_collate_s2" test_collate_s2
    , t "test_collate_p1" test_collate_p1
    , t "test_collate_p2" test_collate_p2
    , t "test_collate_sp" test_collate_sp
    , t "test_collate_a" test_collate_a
    ]
  where t = (,)

test_collate_s1 :: IO ()
test_collate_s1 = D.run ["-text","-lines","micros\\addrspaces.cu","-a=sm_80","-c"]
test_collate_s2 :: IO ()
test_collate_s2 = D.run ["-text","-lines","micros\\addrspaces.cu","-a=sm_80","-c=s"]
test_collate_s3 :: IO ()
test_collate_s3 =
  tCreatesOutputFile "test.sass" $
    D.run ["-text","-lines","micros\\addrspaces.cu","-a=sm_80","-c=s","-o=test.sass"]
test_collate_p1 :: IO ()
test_collate_p1 = D.run ["-text","-lines","micros\\addrspaces.cu","-a=sm_80","-c=p"]
test_collate_p2 :: IO ()
test_collate_p2 =
  tCreatesOutputFile "test.ptx" $
    D.run ["-text","-lines","micros\\addrspaces.cu","-a=sm_80","-c=p","-o=test.ptx"]
test_collate_sp :: IO ()
test_collate_sp = D.run ["-text","-lines","micros\\addrspaces.cu","-a=sm_80","-c=s,p"]
test_collate_a :: IO ()
test_collate_a = D.run ["-text","-lines","micros\\addrspaces.cu","-a=sm_80","-c=a"]
