module Control.Probability.Tree
    ( Tree
    , mkTree
    , fetch
    ) where

import qualified Data.Heap as Heap

-- | A probability tree. These can be built from a frequency list in /O(n log n)/
-- time using 'mkTree', and selecting a value from the tree at random using
-- 'fetch' is done in /O(log n)/ time.
data Tree p a  = Leaf p a | Node p (Tree p a) (Tree p a)

type Queue p a = Heap.MinPrioHeap p a

-- |Build a probability tree from a frequency list.
mkTree :: (Num p, Ord p) => [(a, p)] -> Tree p a
mkTree xs = go (mkQueue xs)
 where
  --go :: Queue (Tree p a) -> Tree p a
  go hp = case Heap.size hp of

      0 -> error "Empty queue"
      1 -> let Just ((_,t), _)    = Heap.view hp in t
      _ -> let Just ((p,s), hp')  = Heap.view hp
               Just ((q,t), hp'') = Heap.view hp'
            in go $ Heap.insert (p+q, Node (p+q) s t) hp''

  mkQueue :: (Ord p) => [(a, p)] -> Queue p (Tree p a)
  mkQueue = Heap.fromList . map swap

  -- swap :: (a, p) -> (p, Tree p a)
  swap (a, p) = (p, Leaf p a)

getP :: Tree p a -> p
getP (Leaf p _)   = p
getP (Node p _ _) = p

{-# INLINE getP #-}

-- | Given a number @p@ in [0,1] this selects a
-- value from a probability tree.
fetch :: (Num p, Ord p) => p -> Tree p a -> a
fetch _ (Leaf _ a)   = a
fetch p (Node _ l r) =
  let q = getP l
  in if p < q
    then fetch  p    l
    else fetch (p-q) r