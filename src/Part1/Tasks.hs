module Part1.Tasks where

import Util(notImplementedYet)
import qualified Data.List as List

fact :: [Double]
fact = [1] ++ zipWith (*) fact [1..]

sincos :: (Int -> Double) -> Double
sincos elem = impl 128 0
    where
        impl 0 s = s + elem 0
        impl k s = impl (k - 1) $ s + elem k

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x' = sincos elem
    where
        x = x' - (fromInteger $ truncate $ x' / 2 / pi) * 2 * pi
        elem k = ((-1) ^ k) * (x ^ (2 * k + 1)) / (fact !! (2 * k + 1))

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x' = sincos elem
    where
        x = x' - (fromInteger $ truncate $ x' / 2 / pi) * 2 * pi
        elem k = ((-1) ^ k) * (x ^ (2 * k)) / (fact !! (2 * k))

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b =
    let a' = abs a in
    let b' = abs b in
    if a' < b' then impl a' b' else impl b' a'
    where
        impl 0 b = b
        impl a b = myGCD (b `mod` a) a

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y =
    and [y >= 1, m >= 1, m <= 12, d >= 1, d <= 40, d <= (monthDays !! fromInteger m)]
  where
    monthDays = [0, 31, 28 + (if isLeap then 1 else 0), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    isLeap = (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x p = impl x p 1
    where
        impl curp 0 acc = acc
        impl curp p acc | even p = impl (curp * curp) (p `div` 2) acc
        impl curp p acc = impl (curp * curp) (p `div` 2) (acc * curp)

primes = sieve [2..]
    where
        sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = head (dropWhile (< x) primes) == x

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea pts =
    let s1 = sum $ zipWith (\a b -> fst a * snd b) pts (tail pts) in
    let s2 = sum $ zipWith (\a b -> fst a * snd b) (tail pts) pts in
    let hdd = head pts in
    let lst = last pts in
    0.5 * abs (s1 + fst lst * snd hdd - s2 - fst hdd * snd lst)

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c =
    let [a', b', c'] = List.sort [a, b, c] in
    if a' + b' <= c'
    then -1
    else
        case (c' * c') `compare` (a' * a' + b' * b') of
            GT -> 0
            EQ -> 2
            LT -> 1
