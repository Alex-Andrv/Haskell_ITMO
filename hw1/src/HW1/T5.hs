module HW1.T5
  ( splitOn,
    joinWith,
  )
where

import Data.List.NonEmpty as NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr split ([] :| [])
  where
    split el (x :| xs)
      | el == sep = [] :| (x : xs)
      | otherwise = (el : x) :| xs

joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep = foldr1 $ \x acc -> x ++ (sep : acc)
