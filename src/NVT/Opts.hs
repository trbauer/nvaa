module NVT.Opts where

import System.Exit


data Opts =
  Opts {
    oVerbosity :: !Int
  , oArch :: !String -- e.g. sm_52, sm_75
  , oOutputFile :: !FilePath
  , oSaveCuBin :: !FilePath -- if input is .cu, this is the .cubin
  , oSavePtx :: !FilePath
--  , oUseCuobjdump :: !Bool -- versus nvdisasm
  , oInputFile :: !FilePath
  , oSourceMapping :: !Bool
  , oFilterAssembly :: !Bool
  , oTextOnly :: !Bool
  , oRDC :: !Bool
  , oNoBits :: !Bool
  , oStdinIs :: !StdinIs
  , oExtraArgs :: ![String]
  } deriving Show
dft_opts :: Opts
dft_opts =
  Opts {
    oVerbosity = 0
  , oArch = "" -- "sm_62" -- "sm_70" -- "sm_52"
  , oOutputFile = ""
  , oSaveCuBin = ""
  , oSavePtx = ""
  , oInputFile = ""
--  , oUseCuobjdump = False
  , oSourceMapping = False
  , oFilterAssembly = True
  , oTextOnly = False
  , oRDC = False
  , oNoBits = False
  , oStdinIs = StdinIsUnknown
  , oExtraArgs = []
  }

data StdinIs =
    StdinIsUnknown
  | StdinIsCu
  | StdinIsPtx
  | StdinIsSass
  deriving (Show,Eq)

dft_opts_75 :: Opts
dft_opts_75 = dft_opts{oArch = "sm_75"}


--------------
debugLn :: Opts -> String -> IO ()
debugLn os
  | oVerbosity os > 1 = putStrLn
  | otherwise = const (return ())
printLn :: Opts -> String -> IO ()
printLn os
  | oVerbosity os >= 0 = putStrLn
  | otherwise = const (return ())
warningLn :: Opts -> String -> IO ()
warningLn os
  | oVerbosity os >= -1 = putStrLn
  | otherwise = const (return ())


fatal :: String -> IO a
fatal msg = die msg
