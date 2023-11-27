module Part5.Tasks where

import Util (notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f (f init x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f init [] = init
myFoldr f init (x : xs) = f x (myFoldr f init xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldl (\acc x -> acc ++ [f x]) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldl (\acc x -> acc ++ f x) []

myConcat :: [[a]] -> [a]
myConcat = myFoldl (++) []

myReverse :: [a] -> [a]
myReverse = myFoldl (flip (:)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldl (\acc x -> if p x then acc ++ [x] else acc) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldl (\(trueAcc, falseAcc) x -> if p x then (trueAcc ++ [x], falseAcc) else (trueAcc, falseAcc ++ [x])) ([], [])
