import System.Environment (getArgs)
import Unzip (optimize)

main = (interact . optimize . read . head) =<< getArgs
