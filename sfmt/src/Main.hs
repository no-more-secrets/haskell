import Data.List.Split    (splitOn)
import System.Environment (getArgs)
import System.FilePath    (takeExtension)
import SmartFormat        (go, Config(..))
import Test               (runTests)

commentsExt :: String -> [String]
commentsExt ".cpp" = ["//","* "]
commentsExt ".hpp" = commentsExt ".cpp"
commentsExt ".c"   = ["* "]
commentsExt ".h"   = commentsExt ".c"
commentsExt ".hs"  = ["--"]
commentsExt ".py"  = ["#"]
commentsExt ".sh"  = ["#"]
commentsExt ".mk"  = ["#"]
commentsExt ".mkh" = ["#"]

commentsFromFileName :: String -> [String]
commentsFromFileName "Makefile" = commentsExt ".mk"
commentsFromFileName "makefile" = commentsExt ".mk"
commentsFromFileName s          = commentsExt (takeExtension s)

mainReal :: [String] -> IO ()
mainReal args = do
    let (co:n:name:_) = args
        commentsOnly  = read co :: Bool
        columns       = read n  :: Int
        filename      = name    :: String

    let config = Config columns (commentsFromFileName filename)
    interact (go commentsOnly config)

program :: [String] -> IO ()
program []   = runTests >> return ()
program args = mainReal args

main :: IO ()
main = getArgs >>= program
