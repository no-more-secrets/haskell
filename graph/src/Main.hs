module Main where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS
import Data.GraphViz
import Data.GraphViz.Commands
import Data.GraphViz.Types
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Text.Printf

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

g :: Gr String String
g = mkGraph (zip [0..6] $ repeat "()")
            [(0,1,"()"),
             (1,2,"()"),
             (2,1,"()"),
             (2,3,"()"),
             (3,4,"()"),
             (4,5,"()"),
             (5,6,"()"),
             (6,6,"()"),
             (3,6,"()")]

defaultVis :: (Graph gr) => gr nl el -> DotGraph Node
defaultVis = graphToDot nonClusteredParams

main :: IO ()
main = do
    quitWithoutGraphviz "graphviz is not installed!"
    prettyPrint g
    printf "order: %d\n" (order g)
    printf "size:  %d\n" (size g)
    printf "nodes: %s\n" (show $ nodes g)
    let reach = map (flip reachable g) (nodes g)
    mapM_ print $ zip (nodes g) reach
    TIO.putStrLn $ printDotGraph (defaultVis g)
    o <- runGraphviz (defaultVis g) Png "graph.png"
    print o
