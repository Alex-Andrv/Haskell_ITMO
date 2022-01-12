module HW1.T6
  ( mcat,
    epart,
  )
where

mcat :: Monoid a => [Maybe a] -> a
mcat = foldr f mempty
  where
    f Nothing acc = acc
    f (Just a) acc = a <> acc

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr f (mempty, mempty)
  where
    f (Left l) (accl, accr) = (l <> accl, accr)
    f (Right r) (accl, accr) = (accl, r <> accr)
