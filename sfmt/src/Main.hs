import System.Environment (getArgs)
import SmartFormat        (go)

main :: IO ()
main = do
    (n:_) <- map read <$> getArgs
    --putStrLn $ replicate n '-'
    interact $ go n
