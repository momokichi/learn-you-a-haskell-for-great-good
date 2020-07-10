doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
  then x
  else x*2

boomBangs xs = [ if x < 10 then "boom!" else "bang!" | x <- xs, odd x]

length' xs = sum [1 | _<- xs]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- factorial :: Integer -> Integer
-- factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2* pi * r

lucky :: Int -> String
lucky 7 = "lucky number seven!"
lucky x = "sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "one"
sayMe 2 = "two"
sayMe 3 = "three"
sayMe 4 = "four"
sayMe 5 = "five"
sayMe x = "not between 1 and 5"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "can't call head ona an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell []  = "the list is empty"
tell (x:[]) = "the list has one element: " ++ show x
tell (x:y:[]) = "the list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "the list is long. the first two elements are: " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter "" = "empty string, whoops"
firstLetter all@(x:xs) = "the first letter of " ++ all ++ " is: " ++ [x]

-- bmiTell :: Double -> String
-- bmiTell bmi
--   | bmi <= 18.5 = "you're underweight, you emo, you!"
--   | bmi <= 25.0 = "you're supposedy normal."
--   | bmi <= 30.0 = "you're fat!"
--   | otherwise = "you're whale."

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT


bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= skinny = "you're underweight, you emo, you!"
  | bmi <= normal = "you're supposedy normal."
  | bmi <= fat = "you're fat!"
  | otherwise = "you're whale."
  where bmi = weight * height ^2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

-- calcBmis :: [(Double, Double)] -> [Double]
-- calcBmis xs = [bmi w h | (w, h) <- xs]
--   where bmi weight height = weight * height ^2

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w/h ^2 ]



cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in sideArea + 2 * topArea

describeList :: [a] -> String
describeList ls = "the list is " ++
  case ls of [] -> "empty."
             [x] -> "a singleto list."
             xs -> "a longer list"