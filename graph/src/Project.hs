module Project (Project(..), getProjects) where

import Data.Maybe            (catMaybes)
import System.FilePath.Posix (takeDirectory, (</>))
import System.Path           (absNormPath)

clRead = "CL.read.1.tlog"

data Project = Project
    { searchPaths :: [FilePath]
    , sources     :: [FilePath]
    , tlog        :: FilePath
    } deriving (Show)
    
getProjects :: IO [Project]
getProjects = do
    sln <- readFile "toplevel/code/src/solution.sln"
    mapM getProject (lines sln)
  where
    getProject :: FilePath -> IO Project
    getProject f = do
        prj <- readFile f
        let (incs:srcs:lib_:_) = lines prj
            paths = catMaybes $ map (absNormPath (takeDirectory f)) $ words $ incs
        return $ Project paths (words srcs) (lib_ </> clRead)
