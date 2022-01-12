module HW1.T4
  ( tfoldr,
  )
where

import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ ans Leaf = ans
tfoldr fun ans (Branch _ left el right) = tfoldr fun (fun el $ tfoldr fun ans right) left
