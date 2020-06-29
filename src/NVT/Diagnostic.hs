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
dFormatWithLines lns = dFormatWithMappings (zip [1..] lns)

dFormatWithCurrentLine :: (Int,String) -> Diagnostic -> String
dFormatWithCurrentLine lm = dFormatWithMappings [lm]

-- format the location with the real line, but
dFormatWithMappings :: [(Int,String)] -> Diagnostic -> String
dFormatWithMappings lms d =
    lFormat (dLoc d) ++ ": " ++ dMessage d ++ context
  where context =
          case ln`lookup`lms of
            Nothing -> ""
            Just ctx ->
              "\n" ++
              ctx ++ "\n" ++
              replicate (cl - 1) ' ' ++ "^"

        ln = lLine (dLoc d)
        cl = lColumn (dLoc d)


