module Part4.Tasks where

import Util (notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a

infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
  reverse (reversed lst)
  where
    reversed REmpty = []
    reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = listToRlist' REmpty
  where
    listToRlist' accumulator [] = accumulator
    listToRlist' accumulator (x : xs) = listToRlist' (accumulator :< x) xs

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
  showsPrec _ = show'
    where
      show' REmpty = id
      show' (xs :< x) = show'   xs . ("," ++) . shows x
  show x = "[" <> show' x <> "]"
    where
      show' REmpty = ""
      show' (REmpty :< x) = show x
      show' (xs :< x) = show' xs <> "," <> show x

instance Eq (ReverseList a) where
  (==) = notImplementedYet
  (/=) = notImplementedYet

instance Semigroup (ReverseList a)

instance Monoid (ReverseList a)

instance Functor ReverseList

instance Applicative ReverseList

instance Monad ReverseList
