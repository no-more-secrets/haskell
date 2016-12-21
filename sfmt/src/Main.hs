import Data.List.Split    (splitOn)
import System.Environment (getArgs)
import System.FilePath    (takeExtension)
import SmartFormat        (go, Config(..))

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

main :: IO ()
main = do
    (co:n:name:_) <- getArgs
    let commentsOnly = read co :: Bool
        columns      = read n  :: Int
        filename     = name    :: String

    let config = Config columns (commentsFromFileName filename)
    interact (go commentsOnly config)
