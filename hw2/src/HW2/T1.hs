module HW2.T1
  ( -- * Datatypes
    Annotated (..)
  , Except (..)
  , Fun (..)
  , List (..)
  , Option (..)
  , Pair (..)
  , Prioritised (..)
  , Quad (..)
  , Stream (..)
  , Tree (..)
    -- * map functions
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapQuad
  , mapStream
  , mapTree
  ) where

import Prelude (($), (.))

data Option a = None | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e

infix 0 :#

data Except e a = Error e | Success a

data Prioritised a = Low a | Medium a | High a

data Stream a = a :> Stream a

infixr 5 :>

data List a = Nil | a :. List a

infixr 5 :.

newtype Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption fun (Some a) = Some (fun a)
mapOption _ None = None

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair fun (P x1 x2) = P (fun x1) (fun x2)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad fun (Q x1 x2 x3 x4) = Q (fun x1) (fun x2) (fun x3) (fun x4)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated fun (a :# e) = fun a :# e

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e) = Error e
mapExcept fun (Success a) = Success (fun a)

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised fun (Low a) = Low (fun a)
mapPrioritised fun (Medium a) = Medium (fun a)
mapPrioritised fun (High a) = High (fun a)

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream fun (x1 :> x2) = fun x1 :> mapStream fun x2

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil = Nil
mapList fun (x1 :. x2) = fun x1 :. mapList fun x2

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F g) = F $ f . g

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree fun (Branch left a right) = Branch (mapTree fun left) (fun a) (mapTree fun right)
