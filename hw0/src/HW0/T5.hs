module HW0.T5
  ( 
    Nat,
    nz,
    ns,
    nplus,
    nmult,
    nFromNatural,
    nToNum,
  )
where

import GHC.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ a = a

ns :: Nat a -> Nat a
ns nat f = nat f . f

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus nat1 nat2 f = nat1 f . nat2 f
nmult nat1 nat2 = nat1 . nat2

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural ni = ns (nFromNatural (ni - 1))

nToNum :: Num a => Nat a -> a
nToNum nat = nat (1 +) 0
