module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    ncmp,
    nFromNatural,
    nToNum,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import Numeric.Natural (Natural)

data N = Z | S N deriving (Show, Eq)

inc :: N -> N
inc Z = S Z
inc (S n) = S (S n)

dec :: N -> N
dec Z = Z
dec (S n) = n

nplus :: N -> N -> N -- addition
nplus a Z = a
nplus a b = nplus (inc a) (dec b)

nmult :: N -> N -> N -- multiplication
nmult Z _ = Z
nmult n x = nplus (nmult (dec n) x) x

nsub :: N -> N -> Maybe N -- subtraction     (Nothing if result is negative)




nsub n Z = Just n
nsub Z _ = Nothing


nsub a b = nsub (dec a) (dec b)

ncmp :: N -> N -> Ordering -- comparison      (Do not derive Ord)
ncmp Z Z = EQ
ncmp _ Z = GT
ncmp Z _ = LT
ncmp a b = ncmp (dec a) (dec b)

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural nat = S (nFromNatural $ pred nat)

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum n = nToNum (dec n) + 1

divide :: N -> N -> (N, N)
divide a b = case nsub a b of
  Nothing -> (Z, a)
  Just el -> addOne $ divide el b
    where
      addOne (f, s) = (inc f, s)

nEven, nOdd :: N -> Bool -- parity checking
nEven n =
  let two = S (S Z)
   in nmod n two == Z
nOdd n = not $ nEven n

ndiv :: N -> N -> N -- integer division
ndiv a b = fst $ divide a b

nmod :: N -> N -> N -- modulo operation
nmod a b = snd $ divide a b
