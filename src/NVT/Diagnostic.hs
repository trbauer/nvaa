module NVT.Diagnostic where

import NVT.Loc

-------------------------------------------------------------------------------
data Diagnostic = 
  Diagnostic {
    dLoc :: !Loc
  , dMessage :: !String    
  } deriving (Show,Eq,Ord)

dCons :: Loc -> String -> Diagnostic
dCons = Diagnostic

dFormat :: Diagnostic -> String
dFormat d = lFormat (dLoc d) ++ ": " ++ dMessage d

dFormatWithLines :: [String] -> Diagnostic -> String
dFormatWithLines lns d = 
    lFormat (dLoc d) ++ ": " ++ dMessage d ++ context
  where context
          | ln - 1 < 0 || ln - 1 >= length lns || cl == 0 = ""
          | otherwise = "\n" ++
            lns !! (ln - 1) ++ "\n" ++
            replicate cl ' ' ++ "^"
        ln = lLine (dLoc d)
        cl = lColumn (dLoc d)