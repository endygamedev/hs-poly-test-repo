module Part1.Tasks where

import Util (notImplementedYet)

taylorSeries :: Double -> Integer -> (Double -> Integer -> Double) -> Double -> Double -> Double
taylorSeries x n termFormula previous target
  | abs (previous - current) <= target = current -- absolute error
  | otherwise = current + taylorSeries x (n + 1) termFormula current target
  where
    current = termFormula x n

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin z = taylorSeries z 0 sinTerm (z + 2 * accuracy) accuracy
  where
    accuracy = 1e-9
    sinTerm :: Double -> Integer -> Double
    sinTerm x n = (-1) ^ n * x ** (2 * fromIntegral n + 1) / fromIntegral (product [1 .. (2 * n + 1)])

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos z = taylorSeries z 0 cosTerm (z + 2 * accuracy) accuracy
  where
    accuracy = 1e-9
    cosTerm :: Double -> Integer -> Double
    cosTerm x n = (-1) ^ n * x ** (2 * fromIntegral n) / fromIntegral (product [1 .. (2 * n)])

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD x 0 = abs x
myGCD x y = myGCD y (x `mod` y)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | day < 1 || month < 1 || month > 12 || year < 1 = False
  | isLeapYear year && month == 2 = day <= 29
  | otherwise = day <= snd (daysInMonth !! fromIntegral (month - 1))
  where
    isLeapYear :: Integer -> Bool
    isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || year `mod` 400 == 0

    daysInMonth :: [(String, Integer)]
    daysInMonth =
      [ ("January", 31),
        ("February", 28),
        ("March", 31),
        ("April", 30),
        ("May", 31),
        ("June", 30),
        ("July", 31),
        ("August", 31),
        ("September", 30),
        ("October", 31),
        ("November", 30),
        ("December", 31)
      ]

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x 0 = 1
myPow x n
  | even n = myPow x (n `div` 2) * myPow x (n `div` 2)
  | otherwise = x * myPow x (n - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False -- 0 and 1 are not prime
  | otherwise = not $ any (\x -> n `mod` x == 0) [2 .. floor (sqrt (fromIntegral n))]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points =
  let (xs, ys) = unzip points
      n = length points
   in 0.5
        * abs
          ( sum [xs !! i * ys !! (i + 1) | i <- [0 .. (n - 2)]]
              + last xs * head ys
              - sum [xs !! (i + 1) * ys !! i | i <- [0 .. (n - 2)]]
              - head xs * last ys
          )

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
  | not $ isTriangle a b c = -1
  | isRectangular a b c = 2
  | isAcute a b c = 0
  | otherwise = 1
  where
    isTriangle :: Double -> Double -> Double -> Bool
    isTriangle a b c =
      a + b > c
        && a + c > b
        && b + c > a

    isRectangular :: Double -> Double -> Double -> Bool
    isRectangular a b c =
      a ^ 2 + b ^ 2 == c ^ 2
        || a ^ 2 + c ^ 2 == b ^ 2
        || b ^ 2 + c ^ 2 == a ^ 2

    isAcute :: Double -> Double -> Double -> Bool
    isAcute a b c =
      a ^ 2 + b ^ 2 < c ^ 2
        || a ^ 2 + c ^ 2 < b ^ 2
        || b ^ 2 + c ^ 2 < a ^ 2
