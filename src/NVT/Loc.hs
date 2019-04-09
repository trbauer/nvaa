module NVT.Loc where

-------------------------------------------------------------------------------
data Loc =
  Loc {
    lFile :: !FilePath
  , lLine :: !Int
  , lColumn :: !Int
  } deriving (Show,Eq,Ord)

lCons :: FilePath -> Int -> Int -> Loc
lCons = Loc

lNONE :: Loc
lNONE = Loc "" 0 0

lFormat :: Loc -> String
lFormat l = maybe_fp ++ show (lLine l) ++ ":" ++ show (lColumn l)
  where maybe_fp = if null (lFile l) then "" else (lFile l++":")