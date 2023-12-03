{-# LANGUAGE FlexibleInstances #-}

module Part6.Tasks where

import Data.List
import Data.Map hiding (lookup, map)
import Util (notImplementedYet)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix
  { sparseMatrixWidth :: Int,
    sparseMatrixHeight :: Int,
    sparseMatrixElements :: Map (Int, Int) a
  }
  deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
  zero :: Int -> Int -> mx
  eye :: Int -> mx
  multiplyMatrix :: mx -> mx -> mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  zero _ _ = 0
  eye _ = 1
  multiplyMatrix x y = x * y

instance Matrix [[Int]] where
  zero n m = [[0 | j <- [0 .. (n - 1)]] | i <- [0 .. (m - 1)]]
  eye n = [[if i == j then 1 else 0 | j <- [0 .. (n - 1)]] | i <- [0 .. (n - 1)]]
  multiplyMatrix x y = [[sum $ zipWith (*) rowX colY | colY <- transpose y] | rowX <- x]

instance Matrix (SparseMatrix Int) where
  zero n m = SparseMatrix n m mempty
  eye n = SparseMatrix n n sparseMatrixElements
    where
      sparseMatrixElements = fromList [((i, i), 1) | i <- [0 .. (n - 1)]]
  multiplyMatrix x y = notImplementedYet

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
