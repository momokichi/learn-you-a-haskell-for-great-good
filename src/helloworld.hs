import           Data.Char
-- main = do
--       putStrLn "hello, what's your name?"
--       name <- getLine
--       putStrLn ("hey " ++ name ++ ", you rock!")

main = do
      putStrLn "what's your first name?"
      firstName <- getLine
      putStrLn "what's your last name?"
      lastName <- getLine
      let bigFirstName = map toUpper firstName
          bigLastName = map toUpper lastName
      putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
