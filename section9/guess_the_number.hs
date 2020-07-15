import           Control.Monad (when)
import           System.Random

main = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
  putStrLn "which number in the range from 1 to 10 am i thinking of?"
  numberString <- getLine
  when (not $ null numberString) $ do
    let number = read numberString
    if randNumber == number
      then putStrLn "you are correct!"
      else putStrLn  $ "sorry, it was " ++ show randNumber
    askForNumber newGen
