{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module EIO ( EIO(..)
           , eTe
           , main_
           , print
           , putStr
           , putStrLn
           , readFile
           ) where

import Control.Exception (try, evaluate, IOException)
import Data.Either       (Either(..))
import System.Exit       (exitFailure)
import System.IO         (hPutStr, stderr)

import Control.Monad     ((=<<), (>>=), (<=<))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import qualified Prelude as P

import Prelude (($), (.), IO, String)

newtype EIO a = EIO { unEIO :: EitherT String IO a }
    deriving (P.Functor, P.Applicative, P.Monad)

runEIO :: EIO a -> IO (Either String a)
runEIO = runEitherT . unEIO

main_ :: EIO () -> IO ()
main_ = fromEitherExit <=< runEIO

fromEitherExit :: Either String a -> IO a
fromEitherExit (Right x) = P.return x
fromEitherExit (Left  s) = do
    hPutStr stderr $ "error (EIO): " P.++ (P.unlines . P.lines) s
    exitFailure

-- "Either To EIO"
eTe :: Either String a -> EIO a
eTe = EIO . hoistEither

putStrLn :: String -> EIO ()
putStrLn = EIO . lift . P.putStrLn

putStr :: String -> EIO ()
putStr = EIO . lift . P.putStr

print :: (P.Show a) => a -> EIO ()
print = EIO . lift . P.print

-- =======================================================
-- IO functions that could throw

tryEIO :: IO a -> EIO a
tryEIO action = (EIO . lift) (try action) >>= \case
    Right x -> EIO $ right x
    Left  e -> EIO $ left $ P.show $ (e :: IOException)

readFile :: String -> EIO String
readFile = tryEIO . P.readFile
