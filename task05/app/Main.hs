module Main where

import           AllSimplePaths
import           Graph
import           System.Environment

main = do
  (filename:start:target:_) <- getArgs
  contents <- readFile filename
  let graph = read contents :: Graph Int
      paths = findAllSimplePaths graph (mkNode (read start)) (mkNode (read target))
  putStr $ pathsToStrings paths
  return ()
