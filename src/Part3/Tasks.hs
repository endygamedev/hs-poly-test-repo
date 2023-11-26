module Part3.Tasks where

import Data.List (group, groupBy, sort, sortOn)
import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n ..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq xs =
  fst . last . sortOn snd . map counts . group . sort . concatMap digits $ xs
  where
    digits :: Int -> [Int]
    digits = map (`mod` 10) . reverse . takeWhile (> 0) . iterate (`div` 10)

    counts :: [Int] -> (Int, Int)
    counts xs = (head xs, length xs)

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : uniq (filter (/= x) xs)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f =
  map (\xs -> (snd . head $ xs, map fst xs)) . groupBy (\(_, x) (_, y) -> x == y) . map (\x -> (x, f x))
