{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)
import System.FilePath    (takeExtension)
import SmartFormat        (go, Config(..))
import Test               (runTests)

defaultCols :: Int
defaultCols = 65

allComments :: [String]
allComments = ["//","* ","--","#","\""]

extComments :: String -> [String]
extComments = \case
    ".hpp" -> extComments ".cpp"
    ".h"   -> extComments ".c"
    ".cpp" -> ["//","* "]
    ".c"   -> ["* "]
    ".hs"  -> ["--"]
    ".py"  -> ["#"]
    ".sh"  -> ["#"]
    ".mk"  -> ["#"]
    ".mkh" -> ["#"]
    ".vim" -> ["\""]

commentsFromFileName :: String -> [String]
commentsFromFileName = \case
    "Makefile" -> extComments ".mk"
    "makefile" -> extComments ".mk"
    ".vimrc"   -> extComments ".vim"
    s          -> extComments (takeExtension s)

main_ :: [String] -> IO ()
main_ ("test":_) = runTests >> return ()
main_ (n:name:_) = interact $ go True  $ config
  where config = Config (read n) (commentsFromFileName name)
main_ (n:_)      = interact $ go False $ config
  where config = Config (read n) allComments
main_ _          = interact $ go False $ config
  where config = Config defaultCols allComments

main :: IO ()
main = getArgs >>= main_
