module Utils ( ascListByValue
             , byLine
             , chunks
             , commonPrefix
             , commonPrefixAll
             , compareWith
             , enumerate
             , enumerate1
             , equating
             , fAnd
             , groupByKey
             , interactLines
             , keep
             , mapFst
             , merge
             , onLines
             , onReverse
             , onWords
             , remove
             , sortPair
             , startsWith
             , strip
             , uncurry3
             , unfoldrList
             , uniform
             , version
             , zipWhile
             , zipWithWhile
             ) where

import Data.List     (groupBy, unfoldr, isPrefixOf)
import Data.Function (on)

version :: Int
version = 1

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith x y = y`isPrefixOf`x

commonPrefix :: (Eq a) => [a] -> [a] -> [a]
commonPrefix x = map fst . takeWhile (uncurry (==)) . zip x

commonPrefixAll :: (Eq a) => [[a]] -> [a]
commonPrefixAll = foldr1 commonPrefix

interactLines :: (String -> String) -> IO ()
interactLines f = interact (f `byLine`)

enumerate :: (Num b, Enum b) => [a] -> [(b, a)]
enumerate = zip [0..]
enumerate1 :: (Num b, Enum b) => [a] -> [(b, a)]
enumerate1 = zip [1..]

merge :: [a] -> [a] -> [a]
merge []     ys = ys
merge (x:xs) ys = x:merge ys xs

zipWhile :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
zipWhile f x y = takeWhile (uncurry f) $ zip x y

zipWithWhile :: (a -> b -> c) -> (a -> b -> Bool) -> [a] -> [b] -> [c]
zipWithWhile f g x y = map (uncurry f) $ zipWhile g x y

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z

fAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
fAnd f g x = (f x) && (g x)

mapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapFst f = map $ \(x,y) -> (f x, y)

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f = map $ \(x,y) -> (x, f y)

-- This is safe since if the list is empty the "head" function
-- will not be called
uniform :: (Eq a) => [a] -> Bool
uniform x = all (==head x) x

ascListByValue :: (a -> b) -> [a] -> [(b,a)]
ascListByValue = undefined

groupByKey :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupByKey f = ascListByValue (f . head) . groupBy (equating f)

keep :: (a -> Bool) -> [a] -> [a]
keep = filter

remove :: (a -> Bool) -> [a] -> [a]
remove f = keep (not . f)

strip :: [Char] -> [Char]
strip = f . f
  where f = reverse . dropWhile (==' ')

equating :: (Eq b) => (a -> b) -> a -> a -> Bool
equating f = (==) `on` f

onLines :: ([String] -> [String]) -> String -> String
onLines f = unlines . f . lines

onWords :: ([String] -> [String]) -> String -> String
onWords f = unwords . f . words

compareWith :: (Ord b) => (a -> b) -> a -> a -> Ordering
compareWith f x y = compare (f x) (f y)

onReverse :: ([a] -> [b]) -> [a] -> [b]
onReverse f = reverse . f . reverse

byLine :: (String -> String) -> String -> String
byLine f = unlines . map f . lines

sortPair :: (Ord a) => (a,a) -> (a,a)
sortPair (x,y) = if x < y then (x,y) else (y,x)

-- An unfoldr function for lists that keeps running until
-- the input (which is a list) becomes empty.
unfoldrList :: ([b] -> (a,[b])) -> [b] -> [a]
unfoldrList f = unfoldr f'
    where
        f' [] = Nothing
        f' xs = Just (f xs)

chunks :: Int -> [a] -> [[a]]
chunks = unfoldrList . splitAt
