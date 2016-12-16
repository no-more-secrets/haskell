import System.Environment (getArgs)
import SmartFormat (fmt)

main = do
    (n:_) <- map read <$> getArgs
    putStrLn (replicate n '-')
    interact (fmt n)
