module Part6.Tests where

import qualified Data.Map

import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Part6.Tasks

unit_eye = do
    eye 1 @?= one
    eye 1 @?= [[one]]
    eye 1 @?= SparseMatrix 1 1 (Data.Map.fromList [((0, 0), one)])
    eye 2 @?= [[one, 0], [0, one]]
    eye 2 @?= SparseMatrix 2 2 (Data.Map.fromList [((0, 0), one), ((1, 1), one)])

    where one :: Int; one = 1

unit_zero = do
    zero 1 1 @?= zz
    zero 2 1 @?= [[zz, zz]]
    zero 2 2 @?= [[zz, zz], [zz, zz]]
    zero 5 5 @?= SparseMatrix 5 5 (Data.Map.fromList ([]::[((Int, Int), Int)]))
    where zz :: Int; zz = 0

unit_det = do
    let
        lst = [[1, 0, 3], [11, 12, 8], [9, -4, 3]]
        spar :: SparseMatrix Int
        spar = mFromList lst
    det lst @?= -388
    det spar @?= det lst

unit_mmul = do
    let
        lstL :: [[Int]]
        lstL = [[1, 0, 3], [11, 12, 8], [9, -4, 3]]
        lstR = reverse [[12, 0, 3], [11, 3, 8], [2, -4, 3]]
        sparL :: SparseMatrix Int
        sparL = mFromList lstL
        sparR = mFromList lstR
        correct = [[38,-4,12],[250,-8,153],[10,-48,4]]
    multiplyMatrix lstL lstR @?= correct
    Data.Map.filter (/= 0) (sparseMatrixElements $ multiplyMatrix sparL sparR) @?= sparseMatrixElements (mFromList correct)
