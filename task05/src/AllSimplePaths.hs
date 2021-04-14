module AllSimplePaths where

import           Control.Monad
import           Debug.Trace
import           Graph

type Path a = [Edge a]

findAllSimplePaths :: (Eq a) => Graph a -> Node a -> Node a -> [Path a]
findAllSimplePaths graph start target = allSimplePaths graph target start [] []

allSimplePaths :: (Eq a) => Graph a -> Node a -> Node a -> [Node a] -> Path a -> [Path a]
allSimplePaths graph target start visited path =
  if target == start
    then [path]
    else let nextToVisit = filter (`notElem` visited) $ neighbourhood graph start
             nextVisited = start : visited
             nextPath node = path ++ [mkEdge start node]
             pathsToTarget node = allSimplePaths graph target node nextVisited $ nextPath node
          in concatMap pathsToTarget nextToVisit

pathToString :: (Show a) => Path a -> String
pathToString path =
  if null path
    then ""
    else (show . fromNode . firstNode . head $ path) ++ concatMap ((" " ++) . show . fromNode . secondNode) path

pathsToStrings :: (Show a) => [Path a] -> String
pathsToStrings paths = unlines $ map pathToString paths
