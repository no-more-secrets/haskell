import Colocate            (colocate)
import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Char           (toLower)
import Data.List           (sortBy, find)
import Data.Maybe          (fromMaybe, fromJust)
import Data.Ord            (comparing)
import System.Directory    (getDirectoryContents)
import System.Environment  (getArgs, getEnv)
import Safe                (headDef, headMay, lastMay, initSafe, maximumByDef)
import Text.ParserCombinators.Parsec (Parser, ParseError, parse, many, char, anyChar)
import Text.Regex          (mkRegex)
import Text.Regex.Posix    (matchTest)
import Utils               (unfoldrList, chunk, fAnd)

-- |======================================================
-- | Bash filtering
-- |======================================================

type BashRegex = String

bashFilter :: BashRegex -> [FilePath] -> [FilePath]
bashFilter bash = filter (notDotFile`fAnd`matches)
  where
    matches = (matchTest . mkRegex . bashToRegex) bash
    notDotFile ('.':_) = False
    notDotFile _       = True

-- Here we ignore errors because any string is a valid bash regex.
bashToRegex :: BashRegex -> String
bashToRegex s = bashCharsToRegex parsed
  where (Right parsed) = parse bashPattern "(unknown)" s

-- |======================================================
-- | Bash Parsing
-- |======================================================

data BashChar = Normal Char | Star | Question
                deriving (Show)

bashPattern :: Parser [BashChar]
bashPattern = many (star <|> question <|> normal)
  where
    star     = char '*' >> return Star
    question = char '?' >> return Question
    normal   = Normal`fmap`anyChar

-- |======================================================
-- | Convert [BashChar] to regex string
-- |======================================================

bashCharsToRegex :: [BashChar] -> String
bashCharsToRegex bashChars = '^' : concat (map bashCharToRegex bashChars) ++ "$"

bash2re = [ ('.',  "\\."),
            ('\\', "\\\\"),
            ('(',  "\\("),
            (')',  "\\)"),
            ('$',  "\\$"),
            ('^',  "\\^"),
            ('+',  "\\+"),
            ('|',  "\\|") ]
            
bashCharToRegex :: BashChar -> String
bashCharToRegex Star       = ".*"
bashCharToRegex Question   = "."
bashCharToRegex (Normal c) = escape c
  where escape c = fromMaybe [c] (lookup c bash2re)

-- |======================================================
-- | Grid formatting
-- |======================================================

formatAcross :: Int -> [String] -> [String]
formatAcross n ss = (initSafe . fromMaybe ss) bestFit
  where
    bestFit :: Maybe [String]
    bestFit = find ((<n) . matrixLength) . matrices $ ss

    matrixLength :: [String] -> Int
    matrixLength = length . maximumByDef [] (comparing length)

    matrices :: [String] -> [[String]]
    matrices xs = take (length xs) (matrix <$> [1..] <*> pure xs)

    matrix :: Int -> [String] -> [String]
    matrix n = colocate . chunk n

-- |======================================================
-- | Main program
-- |======================================================

main :: IO ()
main = do
    columns <- read        <$> getEnv "COLUMNS"
    bash    <- headDef "*" <$> getArgs
    files   <- getDirectoryContents "."

    let sorted = sortBy (comparing (map toLower)) files

    putStr . unlines . formatAcross columns . bashFilter bash $ sorted
