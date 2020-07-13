import           Data.Char
import           Data.List
import qualified Data.Map  as Map

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

-- findKey :: (Eq k) => k -> [(k,v)] -> v
-- findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
-- findKey key [] = Nothing
-- findKey key ((k,v):xs)
--   | key == x = Just v
--   | otherwise = findKey key xs
findKey key xs = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing xs

phoneBook :: Map.Map String String
