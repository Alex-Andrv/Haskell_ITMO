module HW2.T4
  ( -- * Datatypes
    Expr (..),
    Prim (..),
    State (..),

    -- * map functions
    eval,
    joinState,
    mapState,
    modifyState,
    wrapState,
  )
where

import qualified Control.Monad
import HW2.T1
  ( Annotated (..),
    mapAnnotated,
  )

newtype State s a = S {runS :: s -> Annotated s a}

mapState :: (a -> b) -> State s a -> State s b
mapState f (S g) = S (mapAnnotated f . g)

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState (S g) = S $ \old_state ->
  let (S f :# new_state) = g old_state
   in f new_state

modifyState :: (s -> s) -> State s ()
modifyState f = S (\s -> () :# f s)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a
  = Add a a -- (+)
  | Sub a a -- (-)
  | Mul a a -- (*)
  | Div a a -- (/)
  | Abs a -- abs
  | Sgn a -- signum

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  negate x = Op (Mul x (Val (-1)))
  abs a = Op (Abs a)
  signum a = Op (Sgn a)
  fromInteger i = Val (fromInteger i)

instance Fractional Expr where
  x / y = Op (Div x y)
  recip x = Val 1 / x
  fromRational r = Val (fromRational r)

eval :: Expr -> State [Prim Double] Double
eval (Val val) = pure val
eval (Op (Abs x)) = do
  value <- eval x
  modifyState (Abs value :)
  pure (abs value)
eval (Op (Sgn x)) = do
  value <- eval x
  modifyState (Sgn value :)
  pure (signum value)
eval (Op (Add x y)) = do
  value_x <- eval x
  value_y <- eval y
  modifyState (Add value_x value_y :)
  pure (value_x + value_y)
eval (Op (Sub x y)) = do
  value_x <- eval x
  value_y <- eval y
  modifyState (Sub value_x value_y :)
  pure (value_x - value_y)
eval (Op (Mul x y)) = do
  value_x <- eval x
  value_y <- eval y
  modifyState (Mul value_x value_y :)
  pure (value_x * value_y)
eval (Op (Div x y)) = do
  value_x <- eval x
  value_y <- eval y
  modifyState (Div value_x value_y :)
  pure (value_x / value_y)
