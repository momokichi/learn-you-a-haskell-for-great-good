import           System.IO

-- main = do
--   handle <- openFile "baabaa.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

-- main = do
--   withFile "baabaa.txt" ReadMode $ \handle -> do
--     contetns <- hGetContents handle
--     putStr contetns

main = do
  contents <- readFile "baabaa.txt"
  putStr contents
