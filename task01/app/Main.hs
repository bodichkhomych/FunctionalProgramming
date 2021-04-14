module Main where

import           Lib

main :: IO ()
main = do
  point <- readStart
  putStrLn $ maybe "No solution" printDesk $ go [point]
