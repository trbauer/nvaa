module NVT.ListingTypes where

import NVT.IR

data Listing =
  Listing {
    lTextSections :: ![TextSection]
  } deriving (Show,Eq)

data TextSection =
  TextSection {
    tsLoc :: !Loc
  , tsKernelName :: !String
  , tsRegisters :: !Int
  , tsBarriers :: !Int
  , tsAlignment :: !Int -- .align
  , tsBlocks :: ![Block]
  } deriving (Show,Eq)

data Block =
  Block {
    bId :: !Int
  , bLoc :: !Loc
  , bLabels :: ![String]
  , bInsts :: ![Inst]
  } deriving (Show,Eq)

