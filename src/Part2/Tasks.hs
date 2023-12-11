{-# LANGUAGE RecordWildCards #-}
module Part2.Tasks where

import Util(notImplementedYet)

import Data.Function (on)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement x@IntConstant{} = x
replaceVar varName replacement x@Variable{ varName = varName' } = if varName == varName' then replacement else x
replaceVar varName replacement BinaryTerm{..} = (BinaryTerm op `on` (replaceVar varName replacement)) lhv rhv

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate x@IntConstant{} = x
evaluate x@Variable{} = x
evaluate x@BinaryTerm{..} =
   evalHelper (evaluate lhv) (evaluate rhv) fromOp (BinaryTerm op)
   where
      evalHelper :: Term -> Term -> (Int -> Int -> Int) -> (Term -> Term -> Term) -> Term
      evalHelper IntConstant{ intValue = l } IntConstant{ intValue = r } fn _ = IntConstant $ fn l r
      evalHelper l r _ cons = cons l r
      fromOp =
         case op of
            Plus -> (+)
            Minus -> (-)
            Times -> (*)
