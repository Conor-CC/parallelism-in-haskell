module Main where

import Lib

main :: IO ()
main = do
  name <- getLine
  ooft name

ooft str = putStrLn str
