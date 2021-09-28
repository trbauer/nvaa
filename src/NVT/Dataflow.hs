module NVT.Dataflow where

import NVT.IR
import NVT.Parsers.ListingParser

import qualified Data.Set as DS
import qualified Data.Map.Strict as DM
import qualified Data.IntMap.Strict as DIM



-- liveSet :: LiveSet -> DIM.IntMapLiveSet

-- map inst PC to uses that target it
-- DIM.IntMap

-- blockid => DuSet
type DUSets = DIM.IntMap DUSet
type DUSet = DS.Set VR

data VR =
    VRR !R
  | VRP !PR
  | VRUR !UR
  | VRUP !UP
  | VRBR !BR
  deriving (Show,Eq,Ord)


computeDUs :: [Block] -> DUSets
computeDUs bs = DIM.empty


