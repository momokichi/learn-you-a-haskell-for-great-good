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
phoneBook = Map.fromList $
  [
    ("betty", "333-3343"),
    ("boone", "366-7676"),
    ("pasty", "000-4545"),
    ("pasl","555-5342")
  ]

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float
--   deriving (Show)
-- area :: Shape -> Float
-- area (Circle _ _ r)          = pi * r ^2
-- area (Rectangle x1 y1 x2 y2) = (abs $ x2-x1) * (abs $ y2-y1)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x1 y1) r) a b = Circle (Point (x1+a) (y1+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- data Person = Person String String Int Float String String deriving (Show)

data Car = Car {
  company :: String,
  model   :: String,
  year    :: Int
} deriving (Show)

tellCar :: Car -> String
tellCar (Car { company = c, model = m, year = y }) =
  "this " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving (Show)
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k)  `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)


data Person = Person { firstName :: String, lastName :: String, age :: Int} deriving (Eq,Show,Read)

mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
mca = Person {firstName = "Adam", lastName = "Youch", age = 44}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- type PhoneBook2 = [(String, String)]
type PhoneNumber = String
type Name = String
type PhoneBook2 = [(Name, PhoneNumber)]

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left $ "Locker " ++ show lockerNumber ++ " dosen't exist."
  Just (state, code) -> if state /= Taken
      then Right code
      else Left $ "Locker " ++ show lockerNumber ++ " is already taken."

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red    = "Red light"
  show Green  = "Green light"
  show Yellow = "Yellow light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 =  False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo Bool where
  yesno = id

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
  if yesno yesnoVal
      then yesResult
      else noResult
