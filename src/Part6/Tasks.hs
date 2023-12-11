{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, RecordWildCards, BangPatterns, TupleSections #-}
module Part6.Tasks where

import Util (notImplementedYet)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.List as List
import Data.Function (on)
import System.IO.Unsafe (unsafePerformIO)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
    zero' :: Int -> Int -> mx
    eye' :: Int -> mx
    det :: mx -> Int
    mFromList :: [[Int]] -> mx
    mmul :: mx -> mx -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
    zero' _ _ = 0
    eye' _ = 1
    det x = x
    mFromList [[x]] = x
    mmul = (*)
instance Matrix [[Int]] where
    zero' w h = const (replicate w 0) <$> replicate h 0
    eye' w = row <$> [0..w - 1]
        where
            row i = replicate i 0 ++ [1] ++ replicate (w - i - 1) 0
    det [[x]] = x
    det (x : xs) =
        sum $ zipWith (\el mul -> (if even mul then 1 else -1) * el * det (dropC mul)) x [0..]
        where
            dropC i = (\row -> let (pref, suff) = List.splitAt i row in pref ++ tail suff) <$> xs
    mFromList = id
    mmul l r =
        let r' = (List.transpose r) in
        map (\ll -> zipWith (\_ rl -> sum $ zipWith (*) ll rl) ll r') l


instance Matrix (SparseMatrix Int) where
    zero' w h = SparseMatrix w h Map.empty
    eye' w = SparseMatrix w w (Map.fromList [((x, x), 1) | x <- [0..w - 1]])
    det SparseMatrix{..} =
        detFromCorner sparseMatrixElements
        where
            detFromCorner :: Map (Int, Int) Int -> Int
            detFromCorner els =
                if Map.null els
                then 1
                else
                    -- let !_ = unsafePerformIO $ print els in
                    let inRaw = Map.toList $ fst $ Map.split (0, sparseMatrixWidth + 1) $ snd $ Map.split (0, -1) els in
                    List.foldl'
                        (\acc ((ey@0, ex), vl) ->
                            acc + (if even ex then 1 else -1) *
                                vl *
                                detFromCorner (sparseRemove ey ex els))
                        0
                        inRaw
            sparseRemove :: Int -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int
            sparseRemove y x mp =
                Map.fromList $
                    map
                        (\((ky, kx), v) ->
                            (((if ky > y then ky - 1 else ky),
                             (if kx > x then kx - 1 else kx)),
                             v) )
                        (filter
                            (\((ky, kx), v) -> kx /= x && ky /= y)
                            (Map.toList mp))
    mFromList lst =
        SparseMatrix
            (length $ head lst)
            (length lst)
            (List.foldl'
                (\acc (row, y) ->
                    List.foldl'
                        (\acc (el, x) -> if el == 0 then acc else Map.insert (y, x) el acc)
                        acc
                        (zip row [0..]))
                Map.empty
                (zip lst [0..]))
    mmul l' r' =
        let
            l = sparseMatrixElements l'
            r = sparseMatrixElements r'
            rTransp = Map.fromList $ map (\((y, x), v) -> ((x, y), v)) $ Map.toList r
            inspect' = Set.toList $ Map.keysSet l `Set.union` Map.keysSet r
            inspectY = map fst inspect'
            inspectX = map snd inspect'
            inspect = inspectY >>= \y -> (y,) <$> inspectX
            res =
                List.foldl'
                    (\acc (y, x) ->
                        let xs = fst $ Map.split (x, sparseMatrixWidth l') $ snd $ Map.split (x, -1) l in
                        let ys = fst $ Map.split (y, sparseMatrixHeight r') $ snd $ Map.split (y, -1) rTransp in
                        let
                            ans =
                                List.foldl'
                                    (\acc ((xy, xx), vl) -> acc + vl * Map.findWithDefault 0 (y, xx) ys)
                                    0
                                    (Map.toList xs) in
                        Map.insert (x, y) ans acc
                    )
                    Map.empty
                    inspect in
        SparseMatrix (sparseMatrixWidth r') (sparseMatrixHeight l') res

-- Реализуйте следующие функции
-- Единичная матрица
eye :: forall m. Matrix m => Int -> m
eye = eye'
-- I tried doing it with block matrices, but it doesn't work with Int =(

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = zero'

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = mmul

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = det
