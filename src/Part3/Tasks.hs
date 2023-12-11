{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Part3.Tasks where

import Util (notImplementedYet)

import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f x = f <$> [x..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: forall a. (a -> a) -> a -> [a]
ff f x = (\cnt -> x & List.foldl' (.) id (replicate cnt f)) <$> [0..]

digsCount :: Int -> Map.Map Int Int -> Map.Map Int Int
digsCount 0 m = m
digsCount o m = digsCount (o `div` 10) $ Map.adjust (+ 1) (o `mod` 10) m

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq lst =
    snd $ head $ reverse $ List.sort $ map (\(x, y) -> (y, x)) $ Map.toList $
        List.foldl'
            (\acc x ->
                if x == 0
                then Map.adjust (+1) 0 acc
                else digsCount x acc)
            (Map.fromList [(i, 0) | i <- [0..9]])
            lst

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq l = sieve l
    where
        sieve [] = []
        sieve (x : xs) = x : sieve [y | y <- xs, y /= x]

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: forall a k. (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = spin $ (\e -> (f e, e)) <$> l
    where
        spin :: [(k, a)] -> [(k, [a])]
        spin [] = []
        spin ((k, v) : xs) =
            let (eq, neq) = List.partition (\e -> fst e == k) xs in
            (k, v : map snd eq) : spin neq
