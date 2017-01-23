{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative      ((<|>))
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import Control.DeepSeq          (($!!))
import Control.Monad            ((<=<))
import Data.Attoparsec.Text     (parseOnly, skipSpace, char
                                ,string, takeTill)
import Data.Either              (rights)
import Data.List                (delete)
import Data.Maybe               (catMaybes)
import Data.Tuple.Extra         (both)
import Project
import Safe                     (headMay)
import System.Directory.Extra   (listFilesRecursive)
import System.FilePath.Posix    (takeExtension, takeDirectory)
import System.Path              (absNormPath)
import Text.Printf              (printf)
import Utils

import Data.Graph.Inductive.Graph        (mkUGraph)
import Data.Graph.Inductive.PatriciaTree (UGr)
import Data.Graph.Inductive.Query.DFS    (reachable)

import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

srcFolder = "toplevel/code/src"
srcExts   = [".h",".hpp",".cuh",".inl",".c",".cpp",".cu"]

type IncMap = M.Map FilePath [FilePath]

-- Parse a file for all include statements
parseIncludes :: FilePath -> IO [FilePath]
parseIncludes = ((return $!!) . parse) <=< TIO.readFile
  where
    parse = map T.unpack . rights . map (parseOnly ps) . T.lines
    ps =  skipSpace >> char '#' >> skipSpace >> string "include"
       >> skipSpace >> (between '"' '"' <|> between '<' '>')
    l`between`r = char l *> takeTill (==r) <* char r

-- Find all source files under a root folder
findSources :: FilePath -> IO [FilePath]
findSources = fmap (filter isSrc) . listFilesRecursive
  where isSrc = ((`elem`srcExts) . takeExtension)

-- Lookup a key in a map; if not present exit with error
lookup' :: (Ord a, Show a) => M.Map a b -> a -> b
lookup' m k = maybeError msg (M.lookup k m)
  where msg = printf "%s not found in map!" (show k)

project :: IncMap -> Project -> [(FilePath,[FilePath])]
project m pr = cpps`zip`map includes cpps
  where
    cpps = sources pr
    includes f = delete f $ map fromNode
               $ reachable (toNode f) graph

    fs = M.keys m
    xs`to`ys = M.fromList (zip xs ys)

    toNode   = lookup' (fs`to`[1..])
    fromNode = lookup' ([1..]`to`fs)

    -- This is the graph that holds the entire dependency graph
    -- for all sources files in the context of this project.
    graph :: UGr
    graph = mkUGraph [1..length fs] $ map (toNode`both`)
          $ flatten $ map search $ M.toList $ m
      where
        search :: (FilePath, [FilePath]) -> (FilePath, [FilePath])
        search (f, incs) = (f, catMaybes $ map locate $ incs)
          where
            locate :: FilePath -> Maybe FilePath
            locate inc = headMay   $ filter (flip M.member m)
                       $ catMaybes $ map (`absNormPath`inc)
                       $ (takeDirectory f:searchPaths pr)

processProject :: IncMap -> Project -> IO ()
processProject m p = write $ project m p
  where write = writeFileLog (tlog p) . unlines . concat
              . map (\(x,rs) -> (('^':x):rs))

-- ==============================================================
-- Driver
-- ==============================================================

main :: IO ()
main = do
    printf "Getting projects\n"
    prjs     <- getProjects
    printf "Finding sources\n"
    sources  <- strictIO $ findSources srcFolder -- TODO: utils
    printf "Parsing includes\n"
    includes <- strictIO $ mapConcurrently parseIncludes sources
    printf "Constructing incMap\n"
    incMap   <- strictPure $ M.fromList (zip sources includes)
    printf "Processing projects\n"
    printf "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
    mapConcurrently_ (strictIO . processProject incMap) prjs
