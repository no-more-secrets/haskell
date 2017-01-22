module Utils (writeFileLog, strictIO, strictPure) where

import Control.DeepSeq        (($!!), NFData, force)
import Control.Exception.Base (evaluate)
import Text.Printf            (printf)

writeFileLog :: FilePath -> String -> IO ()
writeFileLog f c = printf "writing %s\n" f >> writeFile f c

strictPure :: (NFData a) => a -> IO a
strictPure = evaluate . force

strictIO :: (NFData a) => IO a -> IO a
strictIO a = (return $!!) =<< a
