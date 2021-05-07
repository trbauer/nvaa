module Main where

import qualified NVT.Driver as D
import Test

import System.Environment(getArgs)

main :: IO ()
main = getArgs >>= D.run
