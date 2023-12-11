{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Part3.Tasks where

import Util (notImplementedYet)

import Data.Function ((&))
import qualified Data.List as List

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f x = f <$> [x..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: forall a. (a -> a) -> a -> [a]
ff f x = (\cnt -> x & List.foldl' (.) id (replicate cnt f)) <$> [0..]

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq = notImplementedYet

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq l = sieve l
    where
        sieve [] = []
        sieve (x : xs) = x : sieve [y | y <- xs, y /= x]

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = notImplementedYet
