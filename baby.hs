doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
  then x
  else x*2

boomBangs xs = [ if x < 10 then "boom!" else "bang!" | x <- xs, odd x]

length' xs = sum [1 | _<- xs]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2* pi * r