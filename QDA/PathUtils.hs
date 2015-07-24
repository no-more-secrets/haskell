import Control.Monad.Identity (Identity, runIdentity)
import Data.List (nub, lookup)
import Path (absNormPath)
import System.Directory (doesFileExist)
import System.FilePath.Posix (splitSearchPath, (</>))

includeString = "src/../../../core/core;src/../../../core/core;src/../../../../3rdParty/platforms/common/boost-1.53/include;"

toColon = map toColon'
    where toColon' ';' = ':'
          toColon'  c  =  c

includeStringToFullPaths :: FilePath -> String -> Maybe [FilePath]
includeStringToFullPaths pwd = fmap nub . mapM (absNormPath pwd) . splitSearchPath . toColon

makePathList :: FilePath -> String -> Maybe [FilePath]
makePathList root includeString = includeStringToFullPaths root includeString

makeCandidates :: [FilePath] -> FilePath -> [FilePath]
makeCandidates searchPaths file = toSearchForFile file searchPaths'
    where
        -- just to make sure we are doing this lazily, can remove eventually
        searchPaths' = searchPaths -- ++ repeat "xxxx"
        toSearchForFile f = map (</> f)

-----------------------------------------------------------------------------
-- IO

-- This one is not lazy on account of mapM...
--findInclude :: [FilePath] -> FilePath -> IO (Maybe FilePath)
--findInclude paths file = findFirstExists (makeCandidates paths file)
--    where findFirstExists fs = fmap (lookup True . flip zip fs) . mapM doesFileExist $ fs

findInclude :: (Monad m) => (FilePath -> m Bool) -> [FilePath] -> FilePath -> m (Maybe FilePath)
findInclude fileExists paths file = (findFirstInclude . zip candidates . map fileExists) candidates
    where
        candidates = makeCandidates paths file
        findFirstInclude :: (Monad m) => [(FilePath, m Bool)] -> m (Maybe FilePath)
        findFirstInclude [] = return Nothing
        findFirstInclude ((p,ioB):rest) = do
            exists <- ioB
            if exists then
                return (Just p)
            else
                findFirstInclude rest



-----------------------------------------------------------------------------
-- Unit tests
--
-- Test file system
testFileSystem = [
    "1/2/3/4/5/test.txt",
    "1/2/3/4/test.txt",
    "1/2/3/4/test4.txt",
    "1/2/3/4",
    "1/2/3/test.txt",
    "1/2/3/test2.txt",
    "1/2/3/hello.txt",
    "1/2/test3.txt",
    "1/2/test4.txt",
    "1/xxxtest.txt",
    "test.txt"
    ]
-- 'Fake' function for testing existence of file
fileExistsTest :: FilePath -> Identity Bool
fileExistsTest f = return (f `elem` testFileSystem)

tests = [
    ([],                                           "test.txt",   Nothing),
    (["","1","1/2","1/2/3","1/2/3/4","1/2/3/4/5"], "test.txt",   Just "test.txt"),
    (["1","1/2","1/2/3","1/2/3/4","1/2/3/4/5"],    "test.txt",   Just "1/2/3/test.txt"),
    (["1/2","1/2/3","1/2/3/4","1/2/3/4/5"],        "test.txt",   Just "1/2/3/test.txt"),
    (["1/2/3","1/2/3/4","1/2/3/4/5"],              "test.txt",   Just "1/2/3/test.txt"),
    (["1/2/3/4","1/2/3/4/5"],                      "test.txt",   Just "1/2/3/4/test.txt"),
    (["1/2/3/4/5"],                                "test.txt",   Just "1/2/3/4/5/test.txt"),
    (["1/2/3/4/5/6"],                              "test.txt",   Nothing),
    (["1","1/2","1/2/3","1/2/3/4","1/2/3/4/5",""], "test.txt",   Just "1/2/3/test.txt"),
    (["1/2","1/2/3","1/2/3/4","1/2/3/4/5","","1"], "test.txt",   Just "1/2/3/test.txt"),
    (["1/2/3","1/2/3/4","1/2/3/4/5","","1","1/2"], "test.txt",   Just "1/2/3/test.txt"),
    (["1/2/3/4","1/2/3/4/5","","1","1/2","1/2/3"], "test.txt",   Just "1/2/3/4/test.txt"),
    (["1/2/3/4/5","","1","1/2","1/2/3","1/2/3/4"], "test.txt",   Just "1/2/3/4/5/test.txt"),
    (["1/2/3/4/5/6","","1"],                       "test.txt",   Just "test.txt"),
    (["","1","1/2/3","1/2","1/2/3/4/5","1/2/3/4"], "test.txt",   Just "test.txt"),
    (["1/2/3/4/5","1/2","1","1/2/3","1/2/3/4"],    "test.txt",   Just "1/2/3/4/5/test.txt"),
    (["1/2","1/2/3/4","1/2/3/4/5","1/2/3"],        "test.txt",   Just "1/2/3/4/test.txt"),
    (["1/2/3/4","1/2/3","1/2/3/4/5"],              "test.txt",   Just "1/2/3/4/test.txt"),
    (["1/2/3/4/5","1/2/3/4"],                      "test.txt",   Just "1/2/3/4/5/test.txt"),
    (["","1","1/2","1/2/3","1/2/3/4","1/2/3/4/5"], "test2.txt",  Just "1/2/3/test2.txt"),
    (["1","1/2","1/2/3","1/2/3/4","1/2/3/4/5"],    "test2.txt",  Just "1/2/3/test2.txt"),
    (["1/2","1/2/3","1/2/3/4","1/2/3/4/5"],        "test2.txt",  Just "1/2/3/test2.txt"),
    (["1/2/3","1/2/3/4","1/2/3/4/5"],              "test2.txt",  Just "1/2/3/test2.txt"),
    (["1/2/3/4","1/2/3/4/5"],                      "test2.txt",  Nothing),
    (["1/2/3/4/5"],                                "test2.txt",  Nothing),
    (["1/2/3/4/5/6"],                              "test2.txt",  Nothing),
    (["","1","1/2","1/2/3","1/2/3/4","1/2/3/4/5"], "test2.txt",  Just "1/2/3/test2.txt"),
    (["1","1/2","1/2/3","1/2/3/4","1/2/3/4/5",""], "test2.txt",  Just "1/2/3/test2.txt"),
    (["1/2","1/2/3","1/2/3/4","1/2/3/4/5","","1"], "test4.txt",  Just "1/2/test4.txt"),
    (["1/2/3","1/2/3/4","1/2/3/4/5","","1","1/2"], "test4.txt",  Just "1/2/3/4/test4.txt"),
    (["1/2/3/4","1/2/3/4/5","","1","1/2","1/2/3"], "test4.txt",  Just "1/2/3/4/test4.txt"),
    (["1/2/3/4/5","","1","1/2","1/2/3","1/2/3/4"], "test4.txt",  Just "1/2/test4.txt"),
    (["1/2/3/4/5/6","","1"],                       "test4.txt",  Nothing),
    (["","1","1/2","1/2/3","1/2/3/4","1/2/3/4/5"], "testx.txt",  Nothing),
    (["1/2/3/4","1/2/3/4/5"],                      "testx.txt",  Nothing),
    (["1/2/3/4/5"],                                "testx.txt",  Nothing),
    (["","1","1/2","1/2/3","1/2/3/4","1/2/3/4/5"], "hello.txt",  Just "1/2/3/hello.txt"),
    (["1","1/2","1/2/3","1/2/3/4","1/2/3/4/5"],    "hello.txt",  Just "1/2/3/hello.txt"),
    (["1/2","1/2/3","1/2/3/4","1/2/3/4/5"],        "hello.txt",  Just "1/2/3/hello.txt"),
    (["1/2/3","1/2/3/4","1/2/3/4/5"],              "hello.txt",  Just "1/2/3/hello.txt"),
    (["1/2/3/4","1/2/3/4/5"],                      "hello.txt",  Nothing),
    (["1/2/3/4/5"],                                "hello.txt",  Nothing),
    (["1/2/3/4/5/6"],                              "hello.txt",  Nothing),
    (["","1","1/2","1/2/3","1/2/3/4","1/2/3/4/5"], "hello.txt",  Just "1/2/3/hello.txt"),
    (["1","1/2","1/2/3","1/2/3/4","1/2/3/4/5",""], "hello.txt",  Just "1/2/3/hello.txt"),
    (["1/2","1/2/3","1/2/3/4","1/2/3/4/5","","1"], "hello.txt",  Just "1/2/3/hello.txt"),
    (["1/2/3","1/2/3/4","1/2/3/4/5","","1","1/2"], "hello.txt",  Just "1/2/3/hello.txt"),
    (["1/2/3/4","1/2/3/4/5","","1","1/2","1/2/3"], "hello.txt",  Just "1/2/3/hello.txt"),
    (["1/2/3/4/5","","1","1/2","1/2/3/4"],         "hello.txt",  Nothing),
    (["1/2/3/4/5/6","","1"],                       "hello.txt",  Nothing)
    ]

testsOutput = [(paths,file,answer,(runIdentity $ findInclude fileExistsTest paths file) == answer) | (paths,file,answer) <- tests]
