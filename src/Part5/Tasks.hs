module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ i [] = i
myFoldl f i (x : xs) = myFoldl f (f i x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ i [] = i
myFoldr f i (x : xs) = f x $ myFoldr f i xs

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\x acc -> f x : acc) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr (\x acc -> f x ++ acc) []

myConcat :: [[a]] -> [a]
myConcat = myFoldr (\x acc -> x ++ acc) []

myReverse :: [a] -> [a]
myReverse = myFoldl (\acc x -> x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myConcatMap (\x -> if p x then [x] else [])

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (\x (accT, accF) -> if p x then (x : accT, accF) else (accT, x : accF)) ([], [])
