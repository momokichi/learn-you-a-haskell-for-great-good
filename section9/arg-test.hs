import           Data.List
import           System.Environment

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "the arguments are:"
  mapM putStrLn args
  putStrLn "the progname is:"
  putStrLn progName
