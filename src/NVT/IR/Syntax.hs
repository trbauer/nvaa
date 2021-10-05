module NVT.IR.Syntax where

import Data.Char


class Syntax s where
  format :: s -> String

  formatTypeName :: s -> String
  formatTypeName = const ""


padR :: Int -> String -> String
padR k s = s ++ replicate (k - length s) ' '
padL :: Int -> String -> String
padL k s = replicate (k - length s) ' ' ++ s

lowerCase :: String -> String
lowerCase = map toLower
