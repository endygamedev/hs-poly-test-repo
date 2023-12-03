module Part6.Tests where

import qualified Data.Map
import Part6.Tasks
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

unit_eye = do
  eye 1 @?= one
  eye 1 @?= [[one]]
  eye 1 @?= SparseMatrix 1 1 (Data.Map.fromList [((0, 0), one)])
  eye 2 @?= [[one, 0], [0, one]]
  eye 2 @?= SparseMatrix 2 2 (Data.Map.fromList [((0, 0), one), ((1, 1), one)])
  where
    one :: Int; one = 1

unit_zero = do
  zero 1 1 @?= zz
  zero 2 1 @?= [[zz, zz]]
  zero 2 2 @?= [[zz, zz], [zz, zz]]
  zero 5 5 @?= SparseMatrix 5 5 (Data.Map.fromList ([] :: [((Int, Int), Int)]))
  where
    zz :: Int; zz = 0

unit_multiplyMatrix = do
  multiplyMatrix one one @?= one
  multiplyMatrix mxFirst mxSecond @?= mxResult
  multiplyMatrix sparseMxFirst sparseMxSecond @?= sparseMxResult
  where
    one :: Int
    one = 1

    mxFirst :: [[Int]]
    mxFirst = [[2, 6, 4], [1, 9, 7]]

    mxSecond :: [[Int]]
    mxSecond = [[3, 2], [4, 10], [11, 8]]

    mxResult :: [[Int]]
    mxResult = [[74, 96], [116, 148]]

    sparseMxFirst :: SparseMatrix Int
    sparseMxFirst = SparseMatrix 2 3 (Data.Map.fromList [((0, 0), 2), ((0, 2), 4), ((1, 1), 9)])

    sparseMxSecond :: SparseMatrix Int
    sparseMxSecond = SparseMatrix 3 2 (Data.Map.fromList [((0, 0), 3), ((1, 0), 4), ((1, 1), 10), ((2, 1), 8)])

    sparseMxResult :: SparseMatrix Int
    sparseMxResult = SparseMatrix 2 2 (Data.Map.fromList [((0, 0), 6), ((0, 1), 32), ((1, 0), 36), ((1, 1), 90)])

unit_determinant = do
  determinant one @?= one
  determinant mx @?= 0
  where
    one :: Int
    one = 1

    mx :: [[Int]]
    mx = [[2, 6, 4], [1, 9, 7], [1, 3, 2]]