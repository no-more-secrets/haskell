module Safety
    ( div
    , E
    , read
    ) where

import Data.Either       (Either(..))
import Data.Maybe        (Maybe(..))
import Safe              (readMay)

import qualified Prelude as P

import Prelude (String, (++), ($), Integral)

type E a = Either String a

maybeToE :: String -> Maybe a -> E a
maybeToE _ (Just x) = Right x
maybeToE s Nothing  = Left  s

read :: (P.Read a) => String -> E a
read s = maybeToE msg $ readMay $ s
  where msg = "failed to parse " ++ s

div :: (Integral a) => a -> a -> E a
div _ 0 = Left "attempting to divide by zero"
div x y = Right (x`P.div`y)
