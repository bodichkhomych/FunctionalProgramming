import           AllSimplePaths
import           Graph
import           Test.HUnit

fromNodeTest :: Test
fromNodeTest = test ["test" ~: "(fromNodeTest (Node 9))" ~: 9 ~=? fromNode (Node 9)]

mkNodeTest :: Test
mkNodeTest = test ["test" ~: "(mkNode 3)" ~: Node 3 ~=? mkNode 3]

fromEdgeTest :: Test
fromEdgeTest =
  test
    ["test" ~: "(fromEdgeTest (Edge (Node 11, Node 12)))" ~: (Node 11, Node 12) ~=? fromEdge (Edge (Node 11, Node 12))]

mkEdgeTest :: Test
mkEdgeTest = test ["test" ~: "(mkEdge (Node 8) (Node 1))" ~: Edge (Node 8, Node 1) ~=? mkEdge (Node 8) (Node 1)]

firstNodeTest :: Test
firstNodeTest =
  test ["test" ~: "(firstNode (Edge (Node 25, Node 14)))" ~: Node 25 ~=? firstNode (Edge (Node 25, Node 14))]

secondNodeTest :: Test
secondNodeTest =
  test ["test" ~: "(secondNode (Edge (Node 5, Node 9)))" ~: Node 9 ~=? secondNode (Edge (Node 5, Node 9))]

getAdjacentNodeTest :: Test
getAdjacentNodeTest =
  test
    [ "testFirstNode" ~: "(getAdjacentNode (Node 1) (Edge (Node 1, Node 26)))" ~: Just (Node 26) ~=?
      getAdjacentNode (Node 1) (Edge (Node 1, Node 26))
    , "testSecondNode" ~: "(getAdjacentNode (Node 33) (Edge (Node 45, Node 33)))" ~: Just (Node 45) ~=?
      getAdjacentNode (Node 33) (Edge (Node 45, Node 33))
    , "testOtherwise" ~: "(getAdjacentNode (Node 64) (Edge (Node 51, Node 55))" ~: Nothing ~=?
      getAdjacentNode (Node 64) (Edge (Node 51, Node 55))
    ]

neighbourhoodTest :: Test
neighbourhoodTest =
  test
    [ "testSome" ~:
      "(Graph {graphNodes = [Node 1, Node 2, Node 3], graphEdges = [Edge (Node 1, Node 2), Edge (Node 2, Node 3)]} " ++
      "(Node 2))" ~: [Node 1, Node 3] ~=?
      neighbourhood
        Graph {graphNodes = [Node 1, Node 2, Node 3], graphEdges = [Edge (Node 1, Node 2), Edge (Node 2, Node 3)]}
        (Node 2)
    , "testNone" ~:
      "(Graph {graphNodes = [Node 4, Node 5, Node 7], graphEdges = [Edge (Node 4, Node 5), Edge (Node 4, Node 7)]} " ++
      "(Node 9))" ~: [] ~=?
      neighbourhood
        Graph {graphNodes = [Node 4, Node 5, Node 7], graphEdges = [Edge (Node 4, Node 5), Edge (Node 4, Node 7)]}
        (Node 9)
    ]

findAllSimplePathsTest :: Test
findAllSimplePathsTest =
  test
    [ "test" ~: "(Graph " ++
      "{ graphNodes = [Node 11, Node 21, Node 54, Node 71, Node 6] " ++
      ", graphEdges = " ++
      "[ Edge (Node 11, Node 6) " ++
      ", Edge (Node 11, Node 71) " ++
      ", Edge (Node 21, Node 6) " ++
      ", Edge (Node 54, Node 6) " ++
      ", Edge (Node 71, Node 21) " ++
      ", Edge (Node 71, Node 54) " ++
      "] " ++
      "} " ++
      "(Node 6) " ++
      "(Node 54))" ~:
      [ [Edge (Node 6, Node 11), Edge (Node 11, Node 71), Edge (Node 71, Node 54)]
      , [Edge (Node 6, Node 21), Edge (Node 21, Node 71), Edge (Node 71, Node 54)]
      , [Edge (Node 6, Node 54)]
      ] ~=?
      findAllSimplePaths
        Graph
          { graphNodes = [Node 11, Node 21, Node 54, Node 71, Node 6]
          , graphEdges =
              [ Edge (Node 11, Node 6)
              , Edge (Node 11, Node 71)
              , Edge (Node 21, Node 6)
              , Edge (Node 54, Node 6)
              , Edge (Node 71, Node 21)
              , Edge (Node 71, Node 54)
              ]
          }
        (Node 6)
        (Node 54)
    ]

allSimplePathsTest :: Test
allSimplePathsTest =
  test
    [ "test" ~: "(Graph " ++
      "{ graphNodes = [Node 68, Node 58, Node 41, Node 85] " ++
      ", graphEdges = " ++
      "[ Edge (Node 68, Node 58) " ++
      ", Edge (Node 68, Node 41) " ++
      ", Edge (Node 68, Node 85) " ++
      ", Edge (Node 58, Node 41) " ++
      ", Edge (Node 58, Node 85) " ++
      ", Edge (Node 41, Node 85) " ++
      "] " ++
      "} " ++
      "(Node 68) " ++
      "(Node 85) " ++
      "[] " ++
      "[])" ~:
      [ [Edge (Node 85, Node 68)]
      , [Edge (Node 85, Node 58), Edge (Node 58, Node 68)]
      , [Edge (Node 85, Node 58), Edge (Node 58, Node 41), Edge (Node 41, Node 68)]
      , [Edge (Node 85, Node 41), Edge (Node 41, Node 68)]
      , [Edge (Node 85, Node 41), Edge (Node 41, Node 58), Edge (Node 58, Node 68)]
      ] ~=?
      allSimplePaths
        Graph
          { graphNodes = [Node 68, Node 58, Node 41, Node 85]
          , graphEdges =
              [ Edge (Node 68, Node 58)
              , Edge (Node 68, Node 41)
              , Edge (Node 68, Node 85)
              , Edge (Node 58, Node 41)
              , Edge (Node 58, Node 85)
              , Edge (Node 41, Node 85)
              ]
          }
        (Node 68)
        (Node 85)
        []
        []
    ]

pathToStringTest :: Test
pathToStringTest =
  test
    [ "test" ~: "(pathToString [Edge (Node 58, Node 73), Edge (Node 73, Node 96), Edge (Node 96, Node 90)])" ~:
      "58 73 96 90" ~=?
      pathToString [Edge (Node 58, Node 73), Edge (Node 73, Node 96), Edge (Node 96, Node 90)]
    ]

pathsToStringsTest :: Test
pathsToStringsTest =
  test
    [ "test" ~: "(pathsToStrings " ++
      "[ [Edge (Node 85, Node 68)] " ++
      ", [Edge (Node 85, Node 58), Edge (Node 58, Node 68)] " ++
      ", [Edge (Node 85, Node 58), Edge (Node 58, Node 41), Edge (Node 41, Node 68)] " ++
      ", [Edge (Node 85, Node 41), Edge (Node 41, Node 68)] " ++
      ", [Edge (Node 85, Node 41), Edge (Node 41, Node 58), Edge (Node 58, Node 68)] " ++
      "])" ~: "85 68\n" ++
      "85 58 68\n" ++
      "85 58 41 68\n" ++
      "85 41 68\n" ++
      "85 41 58 68\n" ~=?
      pathsToStrings
        [ [Edge (Node 85, Node 68)]
        , [Edge (Node 85, Node 58), Edge (Node 58, Node 68)]
        , [Edge (Node 85, Node 58), Edge (Node 58, Node 41), Edge (Node 41, Node 68)]
        , [Edge (Node 85, Node 41), Edge (Node 41, Node 68)]
        , [Edge (Node 85, Node 41), Edge (Node 41, Node 58), Edge (Node 58, Node 68)]
        ]
    ]

main :: IO Counts
main = do
  runTestTT fromNodeTest
  runTestTT mkNodeTest
  runTestTT fromEdgeTest
  runTestTT mkEdgeTest
  runTestTT firstNodeTest
  runTestTT secondNodeTest
  runTestTT getAdjacentNodeTest
  runTestTT neighbourhoodTest
  runTestTT findAllSimplePathsTest
  runTestTT allSimplePathsTest
  runTestTT pathToStringTest
  runTestTT pathsToStringsTest
