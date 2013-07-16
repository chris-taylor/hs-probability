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
module Control.Probability.Dist
    ( expectation
    , variance
    , stdDev
    , mean
    , median
    , mode
    , entropyBase
    , entropy
    , grouping
    )
    where

import qualified Data.List as L
import           Data.Ord (comparing)
import           GHC.Float (float2Double, double2Float)

import           Control.Probability.Types

-- |Compute the expectation of a probability distribution.
expectation :: (Probability p, Cast a p) => Dist p a -> p
expectation (Dist m) = sum $ map (\(a,p) -> cast a * p) m

-- |Alias for @expectastion@.
mean :: (Probability p, Cast a p) => Dist p a -> p
mean = expectation

-- |Compute the variance of a probability distribution.
variance :: (Probability p, Cast a p) => Dist p a -> p
variance d@(Dist m) = L.foldl' (\b (a,p) -> b + p * (cast a - mean)^2) 0 m
    where
        mean = expectation d

-- |Compute the standard deviation of a probability distribution.
stdDev :: (Probability p, Floating p, Cast a p) => Dist p a -> p
stdDev = sqrt . variance

-- |Compute the median of a probability distribution.
median :: (Probability p, Ord a) => Dist p a -> a
median (Dist m) = go 0 $ L.sortBy (comparing fst) m
    where
        go _ []         = error "Probabilities do not sum to 1.0 -- Control.Probability.Dist.MEDIAN"
        go p ((a,q):as) = if p + q >= 0.5
            then a
            else go (p+q) as

-- |Compute the mode of a probability distribution.
mode :: Probability p => Dist p a -> a
mode (Dist m) = fst $ L.maximumBy (comparing snd) m

-- |Compute the entropy of a distribution in a particular base. This only makes sens
--  for distributions where equal values can be grouped.
entropyBase :: (Probability p, Floating p, Ord a) => p -> Dist p a -> p
entropyBase k = negate . sum . map f . map snd . grouping . unD
    where
        f 0 = 0
        f p = p * logBase k p

-- |Compute the entropy of a probability distribution. This only makes sense for
--  distributions where equal values can be grouped. This computes the entropy in *nats* --
--  for more general units, see @entropyBase@.
entropy :: (Probability p, Floating p, Ord a) => Dist p a -> p
entropy = entropyBase (exp 1)


-- Class for computing expectations --

class Cast a b where
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
