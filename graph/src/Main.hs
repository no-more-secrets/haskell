module Main where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS
import Data.GraphViz
import Data.GraphViz.Commands
import Data.GraphViz.Types
import Data.List
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Text.Printf

type HeaderGraph = Gr String ()

{-
      (0)
       |
       v
       |
      (1)--<-+
       |     |
       v     ^
       |     |
      (2)-->-+
       |
       v               +--<--+
       |               |     |
      (3)------>------(6)->--+
        \             /
         V           ^
          \         /
          (4)-->--(5)
-}

g :: HeaderGraph
g = mkGraph (zip [0..6] $ repeat "")
            [(0,1,()),
             (1,2,()),
             (2,1,()),
             (2,3,()),
             (3,4,()),
             (4,5,()),
             (5,6,()),
             (6,6,()),
             (3,6,())]

-- Inefficient
hasCycle :: HeaderGraph -> Bool
hasCycle g = hasLoop g || or hasCycle'
  where
    rl         = reachableList g
    hasCycle'  = [n`elem`reachables (delete n ns) | (n,ns) <- rl]
    reachables = concatMap (flip reachable g)

-- For each node find all the nodes which are accessible from
-- that node (obeying directed edges).
reachableList :: HeaderGraph -> [(Node,[Node])]
reachableList g = zip ns $ map (sort . flip reachable g) $ ns
  where ns = nodes g

defaultVis :: HeaderGraph -> DotGraph Node
defaultVis = graphToDot nonClusteredParams

savePng :: HeaderGraph -> FilePath -> IO ()
savePng g f = runGraphviz (defaultVis g) Png f >>= printf msg
  where msg = "saved file %s\n"

printGraph :: HeaderGraph -> IO ()
printGraph = TIO.putStrLn . printDotGraph . defaultVis

printGraphStats :: HeaderGraph -> IO ()
printGraphStats g = do
    printf "headers:     %d\n" (order g)
    printf "edges:       %d\n" (size g)
    printf "isConnected: %s\n" (show $ isConnected g)
    printf "hasLoop:     %s\n" (show $ hasLoop g)
    printf "hasCycle:    %s\n" (show $ hasCycle g)

-- Print a bunch of stuff to stdout and save graph
-- to file
viewGraph :: HeaderGraph -> FilePath -> IO ()
viewGraph g f = do
    mapM_ print (reachableList g)
    prettyPrint     g
    printGraph      g
    printGraphStats g
    savePng         g f

main :: IO ()
main = do
    quitWithoutGraphviz "graphviz is not installed!"
    viewGraph g "graph.png"
