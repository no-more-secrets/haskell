import System.Environment (getArgs)
import Unzip (optimize)
import Utils (onLines)

main = (interact . onLines . optimize . read . head) =<< getArgs
