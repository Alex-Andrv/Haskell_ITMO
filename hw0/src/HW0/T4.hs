module HW0.T4
  ( repeat',
    map',
    fib,
    fac,
  )
where

import Data.Function (fix)
import GHC.Natural (Natural)

repeat' :: a -> [a] -- behaves like Data.List.repeat
repeat' x = fix (x :)

map' :: (a -> b) -> [a] -> [b] -- behaves like Data.List.map
map' fun =
  fix
    ( \m xs ->
        case xs of
          [] -> []
          y : ys -> fun y : m ys
    )

fib :: Natural -> Natural -- computes the n-th Fibonacci number
fib =
  fix
    ( \m ni_1 ni n ->
        case n of
          0 -> ni
          _ -> m ni (ni_1 + ni) (n - 1)
    )  1 0 --- dirty hack

fac :: Natural -> Natural -- computes the factorial
fac =
  fix
    ( \m ni ->
        case ni of
          0 -> 1
          _ -> ni * m (ni - 1)
    )
