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
  determinant :: mx -> Int

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  zero _ _ = 0
  eye _ = 1
  multiplyMatrix x y = x * y
  determinant = id

instance Matrix [[Int]] where
  zero n m = [[0 | j <- [0 .. (n - 1)]] | i <- [0 .. (m - 1)]]
  eye n = [[if i == j then 1 else 0 | j <- [0 .. (n - 1)]] | i <- [0 .. (n - 1)]]
  multiplyMatrix x y = [[sum $ zipWith (*) rowX colY | colY <- transpose y] | rowX <- x]
  determinant x = determinant' x
    where
      determinant' [[x]] = x
      determinant' x =
        let n = length x
         in sum [(-1) ^ j * head x !! j * determinant' (minor 0 j x) | j <- [0 .. (n - 1)]]

      minor i j x = [removeAt j row | (row, rowIndex) <- zip x [0 ..], rowIndex /= i]

      removeAt _ [] = []
      removeAt 0 (_ : xs) = xs
      removeAt n (x : xs) = x : removeAt (n - 1) xs

instance Matrix (SparseMatrix Int) where
  zero n m = SparseMatrix n m mempty
  eye n = SparseMatrix n n sparseMatrixElements
    where
      sparseMatrixElements = fromList [((i, i), 1) | i <- [0 .. (n - 1)]]
  multiplyMatrix x y =
    SparseMatrix
      { sparseMatrixWidth = width,
        sparseMatrixHeight = height,
        sparseMatrixElements = elements
      }
    where
      width = sparseMatrixWidth x
      height = sparseMatrixHeight y

      calculateElement i j x y =
        sum [findWithDefault 0 (i, k) (sparseMatrixElements x) * findWithDefault 0 (k, j) (sparseMatrixElements y) | k <- [0 .. (sparseMatrixHeight x - 1)]]

      elements = fromList [((i, j), calculateElement i j x y) | i <- [0 .. (width - 1)], j <- [0 .. (height - 1)]]

  determinant = notImplementedYet
