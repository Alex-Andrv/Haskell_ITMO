module HW2.T3
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some))
               
import Prelude hiding
  ( concat,
    tail,
  )

joinOption :: Option (Option a) -> Option a
joinOption (Some (Some a)) = Some a
joinOption _ = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success a) = a
joinExcept (Error e) = Error e

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e2) :# e1) = a :# (e1 <> e2)

joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (x :. xs) = concat x
  where
    tail = joinList xs
    concat Nil = tail
    concat (y :. ys) = y :. concat ys

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\i -> extract_and_apply (f i) i)
  where
    extract_and_apply (F fu) i = fu i
