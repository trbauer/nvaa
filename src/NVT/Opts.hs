module NVT.Opts where

import System.Exit


data Opts =
  Opts {
    oArch :: !String -- e.g. sm_52, sm_75
  , oCollateListing :: ![Collate] -- implies -lines
  , oColor :: !Color
  , oExtraCl2ptxArgs :: ![String]
  , oExtraNvccArgs :: ![String]
  , oExtraNvdisasmArgs :: ![String]
  , oFilterAssembly :: !Bool
  , oIncludePaths :: ![FilePath]
  , oInputFile :: !FilePath
  , oOutputFile :: !FilePath
  , oPrintDeps :: !Bool
  , oPrintEncoding :: !Bool
  , oPrintLines :: !Bool
  , oPrintOffsets :: !Bool
  , oRDC :: !Bool
  , oSaveCuBin :: !FilePath -- if input is .cu, this is the .cubin
  , oSavePtx :: !Bool
  , oSavePtxTo :: !FilePath
  , oStdinIs :: !StdinIs
  , oTextSectionsOnly :: !Bool
--  , oUseCuobjdump :: !Bool -- versus nvdisasm
  , oVerbosity :: !Int
  } deriving Show
dft_opts :: Opts
dft_opts =
  Opts {
    oArch = "" -- "sm_62" -- "sm_70" -- "sm_52"
  , oCollateListing = []
  , oColor = ColorAuto
  , oExtraCl2ptxArgs = []
  , oExtraNvccArgs = []
  , oExtraNvdisasmArgs = []
  , oFilterAssembly = True
  , oIncludePaths = []
  , oInputFile = ""
  , oOutputFile = ""
  , oPrintDeps = True
  , oPrintEncoding = False
  , oPrintLines = False
  , oPrintOffsets = False
  , oRDC = False
  , oSaveCuBin = ""
  , oSavePtx = False
  , oSavePtxTo = ""
  , oStdinIs = StdinIsUnknown
  , oTextSectionsOnly = False
--  , oUseCuobjdump = False
  , oVerbosity = 0
  }

data StdinIs =
    StdinIsUnknown
  | StdinIsCl
  | StdinIsCu
  | StdinIsPtx
  | StdinIsSass
  deriving (Show,Eq)

data Collate =
    CollatePTX
  | CollateSASS
  deriving (Show,Eq)

-- dft_opts_75 :: Opts
-- dft_opts_75 = dft_opts{oArch = "sm_75"}
-- dft_opts_80 :: Opts
-- dft_opts_80 = dft_opts{oArch = "sm_80"}
dft_opts_86 :: Opts
dft_opts_86 = dft_opts{oArch = "sm_86"}

data Color =
    ColorAuto | ColorNever | ColorAlways
  deriving (Show,Eq)

--------------
debugLn :: Opts -> String -> IO ()
debugLn os
  | oVerbosity os >= 2 = putStrLn
  | otherwise = const (return ())
verboseLn  :: Opts -> String -> IO ()
verboseLn os
  | oVerbosity os >= 1 = putStrLn
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
