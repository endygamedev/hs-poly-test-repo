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
      show' (xs :< x) = show' xs . ("," ++) . shows x
  show x = "[" <> show' x <> "]"
    where
      show' REmpty = ""
      show' (REmpty :< x) = show x
      show' (xs :< x) = show' xs <> "," <> show x

instance Eq a => Eq (ReverseList a) where
  (==) REmpty REmpty = True
  (==) (xs :< x) (ys :< y) = x == y && xs == ys
  (==) _ _ = False

  (/=) x y = not (x == y)

instance Semigroup (ReverseList a) where
  (<>) x REmpty = x
  (<>) xs (ys :< y) = (xs <> ys) :< y

instance Monoid (ReverseList a) where
  mempty = REmpty

instance Functor ReverseList where
  fmap f REmpty = REmpty
  fmap f (xs :< x) = fmap f xs :< f x

instance Applicative ReverseList where
  pure x = REmpty :< x

  (<*>) REmpty _ = REmpty
  (<*>) (fs :< f) xs = (fs <*> xs) <> fmap f xs

instance Monad ReverseList where
  return x = REmpty :< x

  (>>=) REmpty f = REmpty
  (>>=) (xs :< x) f = (xs >>= f) <> f x
