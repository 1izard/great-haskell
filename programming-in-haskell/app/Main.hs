module Main where

import Lib

main :: IO ()
main = do
  print [(x `mod` 10) + 1 | x <- [-1 .. 11]]
