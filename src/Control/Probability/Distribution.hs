{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Probability.Distribution
  ( Distribution(..)
  , runProb
  , runProb'
  , runMostLikely
  , runMostLikely'
  ) where

import qualified Data.List as L
import qualified Data.Map  as M
import           Data.Map (Map)
import           Data.Ord (comparing)
import           Data.Monoid
import           Control.Monad
import           Control.Applicative

import           Control.Probability.Types
import           Control.Probability.Class
import           Control.Probability.Tree

----------------------------------------------------------
-- Types
----------------------------------------------------------

-- | Type for a probability distribution. The two constructors
-- are for ordered and non-ordered types.
data Distribution p a where
  DistOrd :: (Probability p, Ord a) => Map (Maybe a) p -> Distribution p a
  DistAny :: (Probability p)        => [(Maybe a, p)]  -> Distribution p a

zeroDist  :: (Probability p, Ord a) => Distribution p a
zeroDist  = DistOrd (M.singleton Nothing 1.0)

zeroDist' :: (Probability p)        => Distribution p a
zeroDist' = DistAny [(Nothing,1.0)]

----------------------------------------------------------
-- Instances
----------------------------------------------------------

deriving instance (Show p, Show a) => Show (Distribution p a)

instance Probability p => Functor (Distribution p) where
  fmap = liftM

instance Probability p => Applicative (Distribution p) where
  pure  = return
  (<*>) = ap

instance Probability p => Monad (Distribution p) where
  return  = certainly'

  m >>= f = collect [multProb p (go a) | (a,p) <- toList' m]
   where go = maybe zeroDist' f

instance (Probability p, Ord a, Monoid a) => Monoid (Distribution p a) where
  mempty  = certainly mempty
  mappend = liftP2 mappend

instance (Probability p, Ord a, Num a) => Num (Distribution p a) where
  (+) = liftP2 (+)
  (-) = liftP2 (-)
  (*) = liftP2 (*)
  fromInteger = certainly . fromInteger
  abs         = liftP abs
  signum      = liftP signum

instance (Probability p, Ord a, Fractional a) => Fractional (Distribution p a) where
  (/)          = liftP2 (/)
  recip        = liftP  recip
  fromRational = certainly . fromRational

instance (Probability p, Ord a, Floating a) => Floating (Distribution p a) where
  pi      = certainly pi
  exp     = liftP exp
  sqrt    = liftP sqrt
  log     = liftP log
  (**)    = liftP2 (**)
  logBase = liftP2 logBase
  sin     = liftP sin
  tan     = liftP tan
  cos     = liftP cos
  asin    = liftP asin
  atan    = liftP atan
  acos    = liftP acos
  sinh    = liftP sinh
  tanh    = liftP tanh
  cosh    = liftP cosh
  asinh   = liftP asinh
  atanh   = liftP atanh
  acosh   = liftP acosh

instance (Probability p) => MonadProb p Distribution where
  fromWeights    = DistOrd . M.fromListWith (+) . normalize
  fromWeights'   = DistAny . normalize

  condition test = if test then certainly () else zeroDist

multProb :: (Probability p) => p -> Distribution p a -> Distribution p a
multProb p (DistOrd x) = DistOrd $ M.map (*p) x
multProb p (DistAny x) = DistAny [ (a,p * q) | (a,q) <- x ]

toList' :: Distribution p a -> [(Maybe a,p)]
toList' (DistOrd x) = M.toList x
toList' (DistAny x) = x

toList :: (Probability p, Ord a) => Distribution p a -> [(Maybe a,p)]
toList (DistOrd x) = M.toList x
toList (DistAny x) = grouping x

--fromList :: (Probability p, Ord a) => [(Maybe a,p)] -> Distribution p a
--fromList = DistOrd . M.fromListWith (+) . normalize

--fromList' :: (Probability p) => [(Maybe a,p)] -> Distribution p a
--fromList' = DistAny . normalize

collect :: (Probability p) => [Distribution p a] -> Distribution p a
collect [ ]        = DistAny []
collect [x]        = x
collect (DistOrd x:t) = case collect t of
                        DistOrd y -> DistOrd (M.unionWith (+) x y)
                        DistAny y -> DistOrd (M.unionWith (+) x (M.fromList y))
collect (DistAny x:t) = case collect t of
                        DistOrd y -> DistOrd (M.unionWith (+) (M.fromList x) y)
                        DistAny y -> DistAny (x ++ y)

normalize :: (Probability p) => ProbabilityList p a -> [(Maybe a,p)]
normalize xs = map (\(a,p) -> (Just a, p/total)) xs
 where
  total = sum (map snd xs)

sumProbMaybe :: Probability p => [(Maybe a,p)] -> p
sumProbMaybe = L.foldl' f 0
 where
  f acc (x,p) = case x of
    Just _  -> acc + p
    Nothing -> acc

bayes :: Probability p => [(Maybe a,p)] -> ProbabilityList p a
bayes xs = foldr f [] xs
 where
  total       = sumProbMaybe xs
  f (x,p) acc = case x of
    Just a  -> (a,p/total) : acc
    Nothing -> acc


----------------------------------------------------------
-- User-Facing Methods
----------------------------------------------------------

-- |Convert a @Distribution a@ object into a probability distribution.
runProb :: (Probability p, Ord a) => Distribution p a -> ProbabilityList p a
runProb = bayes . toList

-- |Convert a @Distribution a@ object into a probability distribution, when there is
--  no @Ord@ instance for @a@.
runProb' :: Probability p => Distribution p a -> ProbabilityList p a
runProb' = bayes . toList'

-- |Convert a @Distribution a@ object into a probability distribution, ordering by
--  the most likely results. Must have an @Ord a@ instance.
runMostLikely :: (Probability p, Ord a) => Distribution p a -> ProbabilityList p a
runMostLikely = reorder . bayes . toList

-- |Convert a @Distribution a@ object into a probability distribution, ordering by
--  the most likely results.
runMostLikely' :: (Probability p) => Distribution p a -> ProbabilityList p a
runMostLikely' = reorder . bayes . toList'

reorder :: (Probability p) => ProbabilityList p a -> ProbabilityList p a
reorder = L.sortBy . comparing $ negate . snd


