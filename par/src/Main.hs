{-# LANGUAGE LambdaCase #-}

import Control.Exception (try, evaluate, IOException)
import Control.Monad     ((>=>))
import Data.Either       (Either(..))
import Safe              (readMay)
import System.Exit       (exitFailure)
import Text.Printf       (printf)

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

type EIO a = EitherT String IO a

tryIOEx :: IO a -> EIO a
tryIOEx action = lift (try action) >>= \case
    Right x -> right x
    Left  e -> left $ show $ (e :: IOException)

main_ :: EIO ()
main_ = do
    lift $ putStrLn "(1)"
    x <- tryIOEx $ readFile "input1"
    lift $ putStrLn "(2)"
    y <- tryIOEx $ readFile "input2"
    lift $ putStrLn "(3)"
    result <- hoistEither $ (x`readDivide`y)
    lift $ putStrLn "(4)"
    lift $ print result
    lift $ putStrLn "(5)"

readEither :: (Read a) => String -> Either String a
readEither s = maybeToEither msg $ readMay $ s
  where msg = "failed to parse " ++ s
 
divEither :: (Integral a) => a -> a -> Either String a
divEither _ 0 = Left "attempting to divide by zero"
divEither x y = Right (x`div`y)
 
readDivide :: String -> String -> Either String Int
readDivide xStr yStr = do
    x    <- readEither xStr
    y    <- readEither yStr
    x`divEither`y
-- 
-- tryIOException :: IO a -> IO (Either String a)
-- tryIOException = fmap convert . try
--   where
--     convert (Left e)  = Left $ show $ (e :: IOException)
--     convert (Right x) = Right x
-- 
-- main_ :: IO ()
-- main_ = do
--     x <- tryIOException $ readFile "input1"
--     y <- tryIOException $ readFile "input2"
--     result <- fromEitherExit (x`readDivide`y)
--     print result

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither _ (Just x) = Right x
maybeToEither s Nothing  = Left  s
 
main :: IO ()
main = fromEitherExit =<< runEitherT main_

fromEitherExit :: Either String a -> IO a
fromEitherExit (Right x) = return x
fromEitherExit (Left  s) = do
    putStr $ "Error: " ++ (unlines . lines) s
    exitFailure
 
