import Prelude hiding (foldr1, and)
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Either
import Control.Monad.Writer
import Safe
import System.Random (Random, StdGen, getStdGen, random, randomR, randoms)

import Utils

default (Int)

--------------------------------------------------------------------------------
-- Random

data Where = Here | There | Nowhere
    deriving (Show, Enum, Ord, Eq)

newtype RandomM g a = RandomM { runRandomM :: g -> (a,g) }

instance Functor (RandomM g) where
    f `fmap` (RandomM h) = RandomM $ \g -> let (r,g') = h g
                                            in (f r, g')

instance Applicative (RandomM g) where
    pure x = RandomM $ \g -> (x,g)
    (RandomM f) <*> (RandomM h) = RandomM m
        where m = \g -> let (r,g')  = h g
                            (s,g'') = f g'
                         in (s r, g'')

instance Monad (RandomM g) where
    return = pure
    (RandomM x) >>= f = RandomM m
        where m = \g -> let (a,g') = x g
                            RandomM f' = f a
                         in f' g'

instance Random Where where
    random g        = randomR (Here,Nowhere) g
    randomR (x,y) g = let (res, g') = randomR (fromEnum x, fromEnum y) g
                       in (toEnum res, g')

getRandom :: (Random a) => RandomM StdGen a
getRandom = RandomM random

getRandomR :: (Random a) => (a,a) -> RandomM StdGen a
getRandomR p = RandomM (randomR p)

result :: RandomM StdGen (Where,Where,Where)
result = do
    n1 <- getRandomR (Here,Nowhere)
    n2 <- getRandomR (Here,Nowhere)
    n3 <- getRandomR . sortPair $ (n1,n2) 
    return (n1,n2,n3)

randomMain = print . randomResult =<< getStdGen
    where randomResult :: StdGen -> (Where,Where,Where)
          randomResult = fst . runRandomM result

--------------------------------------------------------------------------------
-- Error handling

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither s (Just result) = Right result
maybeToEither s _ = Left s

printEither (Left s)  = print s
printEither (Right v) = print v

args    = True
theargs = ["1","2","3","x"]

getargs       = maybeToEither "No args available!" (if args then Just theargs else Nothing)
getargat as   = maybeToEither "Invalid index!" .   atMay as
readarg       = maybeToEither "Invalid integer!" . readMay
divideByTwo n = maybeToEither "Error: odd number!" (if even n then Just (n `div` 2) else Nothing)

process = divideByTwo <=< readarg <=< (flip getargat) 3

main = printEither (process =<< getargs)

--------------------------------------------------------------------------------

newtype MonoidF a = MonoidF { getMonoidF :: (a -> a) }
newtype MonoidM m a = MonoidM { getMonoidM :: (a -> m a) }

instance Monoid (MonoidF a) where
    mempty  = MonoidF id
    mappend (MonoidF f) (MonoidF g) = MonoidF (f . g)

instance (MonadPlus m) => Monoid (MonoidM m a) where
    mempty = MonoidM (const mzero)
    mappend (MonoidM f) (MonoidM g) = MonoidM (f >=> g)

-- Monads

foo n = if n<5 then Nothing else Just (n-1)

--chainNf :: (a -> a -> a) -> a -> Int -> a
--chainNf bindf f n = foldr1 bindf · replicate n $ f
chainNf :: (Monoid a) => a -> Int -> a
chainNf f n = foldr1 mappend · replicate n $ f

chainNMonad :: (MonadPlus m) => (a -> m a) -> Int -> a -> m a
chainNMonad f = getMonoidM · chainNf (MonoidM f) -- (>=>)

chainNFunc :: (a -> a) -> Int -> a -> a
chainNFunc f = getMonoidF · chainNf (MonoidF f) -- (.)

--------------------------------------------------------------------------------
-- N Queens algorithm in the list monad

--queens :: (Num a, Enum a, Num b) => a -> b -> [[a]]
--queens m 0 = [[]]
--queens m n = do
--    qs <- queens m (n-1)
--    q  <- [1..m]
--    guard (safe q qs)
--    return (q:qs)
--    where
--        safe q qs = and [ noattack q r | r <- enumerate1 qs ]
--            where noattack q (i,x) = (q /= x) && (abs (q-x) /= i)

--queensNN n = queens n n
--queens88   = queens 8 8

--------------------------------------------------------------------------------
-- main

--main :: IO ()
--main = putStrLn "Hello"

--calcF :: Maybe Int -> Maybe Int -> Maybe (Maybe Int)
calcF year born = fmap (\f -> fmap f born) (fmap (-) year)

--calcA :: Maybe Int -> Maybe Int -> Maybe Int
calcA year born = pure (-) <*> year <*> born

--calcM :: Maybe Int -> Maybe Int -> Maybe Int
calcM year born = year >>= (\y -> born >>= (\b -> return (y-b)))
--calcM year born = do
--    y <- year
--    b <- born
--    return (y-b)

runCalc f = [
    f (Just 5) (Just 2),
    f (Just 5) Nothing,
    f Nothing (Just 2),
    f Nothing Nothing
    ]

--------------------------------------------------------------------------------
-- Composition of Applicatives

(<<**>>) :: (Applicative a, Applicative b) => a (b (s -> t)) -> a (b s) -> a (b t)
x <<**>> y = pure (<*>) <*> x <*> y

(<<$$>>) :: (Applicative a, Applicative b) => (s -> t) -> a (b s) -> a (b t)
x <<$$>> y = pure (<$>) <*> pure x <*> y

purepure :: (Applicative a, Applicative b) => c -> a (b c)
purepure = pure · pure

-- Instead try using the Compose type for generality

newtype MaybeApp a b = MaybeApp { getMaybeApp :: Maybe (a b) }
    deriving (Show)

instance (Functor a) => Functor (MaybeApp a) where
    fmap f (MaybeApp x) = MaybeApp $ (fmap · fmap) f x

instance (Applicative a) => Applicative (MaybeApp a) where
    pure x = MaybeApp $ purepure x
    MaybeApp x <*> MaybeApp y = MaybeApp (x <<**>> y)

fact1 :: Integer -> Writer String Integer
fact1 0 = return 1
fact1 n = do
    let n' = n-1
    tell $ "We've taken one away from " ++ show n ++ "\n"
    m <- fact1 n'
    tell $ "We've called f " ++ show m ++ "\n"
    let r = n*m
    tell $ "We've multiplied " ++ show n ++ " and " ++ show m ++ "\n"
    return r
