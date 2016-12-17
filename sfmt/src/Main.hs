import System.Environment (getArgs)
import SmartFormat (go)

main = do
    (n:_) <- map read <$> getArgs
    --putStrLn (replicate n '-')
    interact (go n)
