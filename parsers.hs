import Text.Parsec hiding (parens)
--import Text.Parsec.Token as PT

type MyParser a = Parsec String () a

sstring :: String -> MyParser String
sstring s = spaces >> string s >> spaces >> return s

withSpace :: MyParser a -> MyParser a
withSpace p = do
    spaces
    res <- p
    spaces
    return res

exhaust :: MyParser String -> MyParser String
exhaust p = do
    res <- p
    eof
    return res

parens :: String -> MyParser a -> String -> MyParser a
parens l p r = between (sstring l) (sstring r) p

nested :: MyParser String
nested = fmap unwords (stuff`sepBy1`spaces)
  where stuff = parens "(" nested ")" <|> many1 (withSpace alphaNum)

parser :: MyParser String
parser = exhaust nested
