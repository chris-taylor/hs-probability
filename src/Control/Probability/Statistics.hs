{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- |This modules contains the 'Dist' type which is a catch-all representation of
--  a probability distribution. We don't provide a 'Monad' instance as it would
--  be very inefficient, but we define functions like 'expectation' and 'variance'
--  on this type.
--
--  The general pattern is to work with a 'ProbMonad' or 'MonteCarlo' object for
--  all calculations, and call the appropriate 'runProb' method to convert the
--  result of the probabilistic calculation into a 'Dist' in order to query its
--  expectation, variance etc.
module Control.Probability.Statistics
    ( expectation
    , variance
    , stdDev
    , mean
    , median
    , mode
    , entropyBase
    , entropy
    )
    where

import qualified Data.List as L
import           Data.Ord   (comparing)
import           GHC.Float  (float2Double, double2Float)

import           Control.Probability.Types
import           Control.Probability.Distribution

-- |Compute the expectation of a probability distribution.
expectation :: (Probability p, Cast a p) => Distribution p a -> p
expectation = go . runProb
 where
  go = sum . map (\(a,p) -> cast a * p)

-- |Alias for 'expectation'.
mean :: (Probability p, Cast a p) => Distribution p a -> p
mean = expectation

-- | Compute the variance of a probability distribution.
variance :: (Probability p, Cast a p) => Distribution p a -> p
variance = go . runProb
 where
  go x = L.foldl' (\b (a,p) -> b + p * (cast a - mu)^2) 0 x
   where
    mu = sum $ map (\(a,p) -> cast a * p) x

-- | Compute the standard deviation of a probability distribution.
stdDev :: (Floating p, Probability p, Cast a p) => Distribution p a -> p
stdDev d = sqrt (variance d)

-- | Compute the median of a probability distribution.
median :: (Probability p, Ord a) => Distribution p a -> a
median = go 0 . L.sortBy (comparing fst) . runProb
 where
  go _ []         = error "Probabilities do not sum to 1.0 -- Control.Probability.Dist.MEDIAN"
  go p ((a,q):as) = if p + q >= 0.5
      then a
      else go (p+q) as

-- | Compute the mode of a probability distribution.
mode :: (Probability p, Ord a) => Distribution p a -> a
mode = fst . L.maximumBy (comparing snd) . runProb

-- | Compute the entropy of a probability distribution in the given base.
-- This only makes sense for distributions where equal values can be grouped,
-- hence the 'Ord' constraint.
entropyBase :: (Probability p, Floating p, Ord a) => p -> Distribution p a -> p
entropyBase k = negate . sum . map (f . snd) . grouping . runProb
 where
  f 0 = 0
  f p = p * logBase k p

-- | Compute the entropy of a probability distribution in natural units.
-- This only makes sense for distributions where equal values can be grouped,
-- hence the 'Ord' constraint.
entropy :: (Probability p, Floating p, Ord a) => Distribution p a -> p
entropy d = entropyBase (exp 1.0) d


{---------------------------------
  Numeric casts
----------------------------------}

-- | Class describing numeric types that can be cast (possibly with some loss
-- of accuracy) to another numeric type.
class Ord a => Cast a b where
    cast :: a -> b

instance Cast Double   Double    where cast = id
instance Cast Float    Double    where cast = float2Double
instance Cast Int      Double    where cast = fromIntegral
instance Cast Integer  Double    where cast = fromIntegral
instance Cast Rational Double    where cast = fromRational

instance Cast Double   Rational  where cast = toRational
instance Cast Float    Rational  where cast = toRational
instance Cast Int      Rational  where cast = toRational
instance Cast Integer  Rational  where cast = toRational
instance Cast Rational Rational  where cast = id

instance Cast Double   Float     where cast = double2Float
instance Cast Float    Float     where cast = id
instance Cast Int      Float     where cast = fromIntegral
instance Cast Integer  Float     where cast = fromIntegral
instance Cast Rational Float     where cast = fromRational
