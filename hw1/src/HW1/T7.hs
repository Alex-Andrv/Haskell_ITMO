module HW1.T7
  ( ListPlus (..),
    Inclusive (..),
    DotString (..),
    Fun (..),
  )
where

data ListPlus a = a :+ ListPlus a | Last a deriving (Show)

infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last a) r = a :+ r
  (<>) (a :+ xs) r = a :+ (xs <> r)

data Inclusive a b = This a | That b | Both a b deriving (Show)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This x) (This y) = This (x <> y)
  (<>) (This x) (That y) = Both x y
  (<>) (This x) (Both y z) = Both (x <> y) z
  (<>) (That x) (That y) = That (x <> y)
  (<>) (That x) (This y) = Both y x
  (<>) (That x) (Both y z) = Both y (x <> z)
  (<>) (Both x y) (This z) = Both (x <> z) y
  (<>) (Both x y) (That z) = Both x (y <> z)
  (<>) (Both x y) (Both z w) = Both (x <> z) (y <> w)

newtype DotString = DS String deriving (Show)

instance Semigroup DotString where
  (<>) a (DS "") = a
  (<>) (DS "") b = b
  (<>) (DS a) (DS b) = DS (a ++ ('.' : b))

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F f) (F s) = F (f . s)

instance Monoid (Fun a) where
  mempty = F id
