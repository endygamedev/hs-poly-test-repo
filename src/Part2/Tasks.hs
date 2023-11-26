module Part2.Tasks where

import Lambdas (replace)
import Util (notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term
  = IntConstant {intValue :: Int} -- числовая константа
  | Variable {varName :: String} -- переменная
  | BinaryTerm {op :: BinaryOp, lhv :: Term, rhv :: Term} -- бинарная операция
  deriving (Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 6 |+|

(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus

infixl 6 |-|

(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus

infixl 7 |*|

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (IntConstant intValue) = IntConstant intValue
replaceVar varName replacement (Variable variable) =
  if variable == varName
    then replacement
    else Variable variable
replaceVar varName replacement (BinaryTerm op lhv rhv) =
  BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (IntConstant intValue) = IntConstant intValue
evaluate (Variable varName) = Variable varName
evaluate (BinaryTerm op lhv rhv) =
  case (evaluate lhv, evaluate rhv) of
    (IntConstant lhvIntValue, IntConstant rhvIntValue) ->
      case op of
        Plus -> IntConstant (lhvIntValue + rhvIntValue)
        Minus -> IntConstant (lhvIntValue - rhvIntValue)
        Times -> IntConstant (lhvIntValue * rhvIntValue)
    (_, _) -> BinaryTerm op lhv rhv
