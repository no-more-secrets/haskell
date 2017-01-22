{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative        ((<|>))
import Control.Concurrent.Async   (mapConcurrently, mapConcurrently_)
import Control.Monad              ((<=<))
import Data.Attoparsec.Text       (parseOnly, skipSpace, char, string, takeTill)
import Data.Either                (rights)
import Data.List                  (delete)
import Data.Maybe                 (catMaybes)
import Project
import Safe                       (headMay)
import System.FilePath.Posix      ((</>), takeExtension, takeDirectory)
import System.Path                (absNormPath)
import Text.Printf                (printf)
import Utils

import Data.Graph.Inductive.Graph        (Node, insEdges, mkUGraph)
import Data.Graph.Inductive.PatriciaTree (UGr)
import Data.Graph.Inductive.Query.DFS    (reachable)

import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified System.FilePath.Find as F

-- ==============================================================
-- Constants
-- ==============================================================

srcFolder = "toplevel/code/src"
srcExts   = [".h",".hpp",".cuh",".inl",".c",".cpp",".cu"]
outFile   = "CL.read.1.tlog"

-- ==============================================================
-- Graph
-- ==============================================================

type HeaderGraph = UGr
type FromNodeMap = M.Map Node FilePath
type ToNodeMap   = M.Map FilePath Node
type GraphInfo   = (HeaderGraph, FromNodeMap, ToNodeMap)
type IncMap      = M.Map FilePath [FilePath]

graph :: [FilePath] -> GraphInfo
graph fs = (mkUGraph [1..length fs] [], [1..]`to`fs, fs`to`[1..])
  where xs`to`ys = M.fromList (zip xs ys)

-- ==============================================================
-- Parsing
-- ==============================================================

parseIncludes :: FilePath -> IO [FilePath]
parseIncludes = (return . go) <=< TIO.readFile
  where
    go = map T.unpack . rights . map (parseOnly ps) . T.lines
    ps =  skipSpace >> char '#'
       >> skipSpace >> string "include"
       >> skipSpace >> (between '"' '"' <|> between '<' '>')
    l`between`r = char l *> takeTill (==r) <* char r

-- ==============================================================
-- Finding
-- ==============================================================

findSources :: FilePath -> IO [FilePath]
findSources = F.find (pure True) p
  where p = ((`elem`srcExts) . takeExtension) <$> F.fileName

-- ==============================================================
-- Single Project
-- ==============================================================

findFile :: IncMap -> [FilePath] -> FilePath -> Maybe FilePath
findFile m paths f = do
    let candidates = catMaybes $ map (`absNormPath`f) $ paths
    headMay $ filter (flip M.member m) $ candidates

search :: IncMap -> [FilePath] -> (FilePath, [FilePath]) -> (FilePath, [FilePath])
search m paths (f, includedFiles) = let
    here = takeDirectory f
    paths' = here:paths
 in (f, catMaybes $ map (findFile m paths') $ includedFiles)

doProject :: IncMap -> Project -> [(FilePath, FilePath)]
doProject m p = flatten $ map (search m (searchPaths p)) $ M.toList $ m
  where
    flatten :: (Monad m) => m (a, m b) -> m (a,b)
    flatten = (>>= uncurry (fmap . (,)))

lookupErr :: (Ord a, Show a) => M.Map a b -> a -> b
lookupErr m k = case M.lookup k m of
    Nothing  -> error $ printf "%s not found!" (show k)
    Just val -> val

doProjectGraph :: GraphInfo -> IncMap -> Project -> HeaderGraph
doProjectGraph (gr, fromNodeMap, toNodeMap) im pr = let
    get = lookupErr toNodeMap
 in insEdges [(get a,get b,()) | (a,b) <- doProject im pr] gr

doProjectAll :: GraphInfo -> IncMap -> Project -> [(FilePath,[FilePath])]
doProjectAll grInfo@(gr, fromNodeMap, toNodeMap) im pr = let
    gr' = doProjectGraph grInfo im pr
    get = lookupErr toNodeMap
    put = lookupErr fromNodeMap
 in [(f,(delete f) $ map put $ reachable (get f) $ gr') | f <- sources pr]

-- ==============================================================
-- IO Project driver
-- ==============================================================

processProject :: GraphInfo -> IncMap -> Project -> IO ()
processProject grInfo m p = writeProjectOutput p results
  where
    results = doProjectAll grInfo m p
    writeProjectOutput :: Project -> [(FilePath,[FilePath])] -> IO ()
    writeProjectOutput pr = writeFileLog (lib pr </> outFile) . anchored
      where
        anchored = unlines . concat . map (\(x,rs) -> (('^':x):rs))


-- ==============================================================
-- Driver
-- ==============================================================

main :: IO ()
main = do
    printf "Getting projects\n"
    prjs     <- getProjects
    printf "Finding sources\n"
    sources  <- strictIO $ findSources srcFolder
    printf "Parsing includes\n"
    includes <- strictIO $ mapConcurrently parseIncludes sources
    printf "Constructing initial graph\n"
    grInfo   <- strictPure $ graph sources
    printf "Constructing incMap\n"
    incMap   <- strictPure $ M.fromList (zip sources includes)
    printf "Processing projects\n"
    printf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
    mapConcurrently_ (processProject grInfo incMap) prjs
