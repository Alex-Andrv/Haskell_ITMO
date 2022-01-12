module HW1.T3
  ( Tree (..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList,
  )
where

data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a) deriving (Show)

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (cachedSize, _) _ _ _) = cachedSize

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (_, height) _ _ _) = height

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ left el right) = case compare x el of
  LT -> tmember x left
  GT -> tmember x right
  _ -> True

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = createBranch Leaf x Leaf
tinsert x tree@(Branch _ left el right) = case compare x el of
  LT -> mkBranch (tinsert x left) el right
  GT -> mkBranch left el (tinsert x right)
  _ -> tree

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf

rotateLeft, rotateRight :: Tree a -> a -> Tree a -> Tree a
rotateLeft left el (Branch _ rLeft rEl rRight) = createBranch (createBranch left el rLeft) rEl rRight
rotateLeft _ _ _ = undefined
rotateRight (Branch _ lLeft lEl lRight) el right = createBranch lLeft lEl (createBranch lRight el right)
rotateRight _ _ _ = undefined

rotateRightIfNeed, rotateLeftIfNeed :: Tree a -> Tree a
rotateRightIfNeed subTree@(Branch _ left el right) =
  if balance left right > 0
    then rotateRight left el right
    else subTree
rotateRightIfNeed _ = undefined
rotateLeftIfNeed subtree@(Branch _ left el right) =
  if balance left right < 0
    then rotateLeft left el right
    else subtree
rotateLeftIfNeed _ = undefined

balance :: Tree a -> Tree a -> Int
balance left right = tdepth left - tdepth right

createBranch :: Tree a -> a -> Tree a -> Tree a
createBranch left el right = Branch (getTupleScale left right) left el right
  where
    getTupleScale :: Tree a -> Tree a -> (Int, Int)
    getTupleScale l r = (countSize l r, countDepth l r)
    countDepth, countSize :: Tree a -> Tree a -> Int
    countDepth l r = 1 + max (tdepth l) (tdepth r)
    countSize l r = 1 + tsize l + tsize r

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left el right = case balance left right of
  2 -> rotateRight (rotateLeftIfNeed left) el right
  -2 -> rotateLeft left el (rotateRightIfNeed right)
  _ -> createBranch left el right
