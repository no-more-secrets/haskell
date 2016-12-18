import System.Environment (getArgs)
import SmartFormat        (go)

main :: IO ()
main = do
    (n:_) <- map read <$> getArgs
    interact $ go n
