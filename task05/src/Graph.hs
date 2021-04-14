module Graph where

import           Data.Maybe

data Graph a =
  Graph
    { graphNodes :: [Node a]
    , graphEdges :: [Edge a]
    }
  deriving (Eq, Show, Read)

newtype Node a =
  Node a
  deriving (Eq, Show, Read)

newtype Edge a =
  Edge (Node a, Node a)
  deriving (Eq, Show, Read)

fromNode :: Node a -> a
fromNode (Node a) = a

mkNode :: a -> Node a
mkNode = Node

fromEdge :: Edge a -> (Node a, Node a)
fromEdge (Edge pair) = pair

mkEdge :: Node a -> Node a -> Edge a
mkEdge x y = Edge (x, y)

firstNode :: Edge a -> Node a
firstNode = fst . fromEdge

secondNode :: Edge a -> Node a
secondNode = snd . fromEdge

getAdjacentNode :: (Eq a) => Node a -> Edge a -> Maybe (Node a)
getAdjacentNode node edge
  | node == firstNode edge = Just (secondNode edge)
  | node == secondNode edge = Just (firstNode edge)
  | otherwise = Nothing

neighbourhood :: (Eq a) => Graph a -> Node a -> [Node a]
neighbourhood graph node = mapMaybe (getAdjacentNode node) $ graphEdges graph
