module Main where

import           Data.ContinuedFraction

main :: IO ()
main
  = putStrLn
  . ("e = Glenn[2] = " ++)
  . show
  . approx
  . cut 100
  $ glenn 2
