module Control.Probability.Dist
    ( Dist(..)
    , Prob
    , expectation
    , variance
    , stdDev
    , entropyBase
    , entropy
    , grouping
    )
    where

import qualified Data.List as L
import qualified Data.Map  as M

------------------
-- The Dist type
------------------

--  This is what you operate on when you want to know an expectation, variance etc.

-- |This allows the type used to represent probabilities to be easily swapped out.
--  In the future I may make this type variable.
type    Prob = Double

-- |Probability distributions are just a wrapper around a list, pairing values
--  with probabilities. 
newtype Dist a = Dist { unD :: [(a,Prob)] } deriving Show


-- |Compute the expectation of a probability distribution.
expectation :: Dist Prob -> Prob
expectation (Dist m) = L.foldl' (\b (a,p) -> p*a + b) 0 m

-- |Compute the variance of a probability distribution.
variance :: Dist Prob -> Prob
variance d@(Dist m) = L.foldl' (\b (a,p) -> b + p * (a-mean)^2) 0 m
    where
        mean = expectation d

-- |Compute the standard deviation of a probability distribution.
stdDev :: Dist Prob -> Prob
stdDev = sqrt . variance

-- |Compute the entropy of a distribution in a particular base. This only makes sens
--  for distributions where equal values can be grouped.
entropyBase :: Ord a => Prob -> Dist a -> Prob
entropyBase k = negate . sum . map f . map snd . grouping . unD
    where
        f 0 = 0
        f p = p * logBase k p

-- |Compute the entropy of a probability distribution. This only makes sense for
--  distributions where equal values can be grouped. This computes the entropy in *nats* --
--  for more general units, see @entropyBase@.
entropy :: Ord a => Dist a -> Prob
entropy = entropyBase (exp 1)

-- Utility function to group equal elements in a list, by converting the list
-- to a @Map@ and back.
grouping :: Ord a => [(a,Prob)] -> [(a,Prob)]
grouping = M.toList . M.fromListWith (+)

