{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)
import System.FilePath    (takeExtension)
import SmartFormat        (go, Config(..))
import Test               (runTests)

allComments :: [String]
allComments = ["//","* ","--","#"]

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

commentsFromFileName :: String -> [String]
commentsFromFileName = \case
    "Makefile" -> extComments ".mk"
    "makefile" -> extComments ".mk"
    s          -> extComments (takeExtension s)

main_ :: [String] -> IO ()
main_ (n:name:_) = interact $ go True  $ config
  where config = Config (read n) (commentsFromFileName name)
main_ (n:_)      = interact $ go False $ config
  where config = Config (read n) allComments

main :: IO ()
main = getArgs >>= \case
    []   -> runTests >> return ()
    args -> main_ args
