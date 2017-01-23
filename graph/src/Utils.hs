module Utils (maybeError, writeFileLog, strictIO, strictPure, flatten) where

import Control.DeepSeq        (($!!), NFData, force)
import Control.Exception.Base (evaluate)
import Text.Printf            (printf)

writeFileLog :: FilePath -> String -> IO ()
writeFileLog f c = printf "writing %s\n" f >> writeFile f c

strictPure :: (NFData a) => a -> IO a
strictPure = evaluate . force

strictIO :: (NFData a) => IO a -> IO a
strictIO a = (return $!!) =<< a

flatten :: (Monad m) => m (a, m b) -> m (a,b)
flatten = (>>= uncurry (fmap . (,)))

maybeError :: String -> Maybe a -> a
maybeError msg x = case x of
    Nothing  -> error msg
    Just val -> val
