module HW2.T5
  ( -- * Datatypes
    EvaluationError (..),
    ExceptState (..),

    -- * map functions
    eval,
    joinExceptState,
    mapExceptState,
    modifyExceptState,
    wrapExceptState,
    throwExceptState,
  )
where

import Control.Monad (ap)
import HW2.T1 (Annotated ((:#)), Except (Error, Success), mapAnnotated, mapExcept)
import HW2.T4 (Expr (Op, Val), Prim (Abs, Add, Div, Mul, Sgn, Sub))

data ExceptState e s a = ES
  { runES :: s -> Except e (Annotated s a)
  }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g) = ES (mapExcept (mapAnnotated f) . g)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES (Success . (a :#))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) =
  ES
    ( \old_state -> case f old_state of
        Error e -> Error e
        Success (a :# new_state) -> runES a new_state
    )

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState fun = ES (Success . (() :#) . fun)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val val) = pure val
eval (Op (Abs x)) = do
  value <- eval x
  modifyExceptState (Abs value :)
  pure (abs value)
eval (Op (Sgn x)) = do
  value <- eval x
  modifyExceptState (Sgn value :)
  pure (signum value)
eval (Op (Add x y)) = do
  value_x <- eval x
  value_y <- eval y
  modifyExceptState (Add value_x value_y :)
  pure (value_x + value_y)
eval (Op (Sub x y)) = do
  value_x <- eval x
  value_y <- eval y
  modifyExceptState (Sub value_x value_y :)
  pure (value_x - value_y)
eval (Op (Mul x y)) = do
  value_x <- eval x
  value_y <- eval y
  modifyExceptState (Mul value_x value_y :)
  pure (value_x * value_y)
eval (Op (Div x y)) = do
  value_x <- eval x
  value_y <- eval y
  if value_y == 0
    then throwExceptState DivideByZero
    else do
      modifyExceptState (Div value_x value_y :)
      pure (value_x / value_y)
