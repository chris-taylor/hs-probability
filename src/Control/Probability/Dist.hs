module Control.Probability.Dist
    ( Dist(..)
    , expectation
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
import qualified Data.Map  as M
import           Data.Ord (comparing)

-----------------------------------------------------------------
-- The Dist type
-----------------------------------------------------------------

-- |Probability distributions are just a wrapper around a list, pairing values
--  with probabilities. 
newtype Dist p a = Dist { unD :: [(a,p)] } deriving Show


-- |Compute the expectation of a probability distribution.
expectation :: (Fractional p) => Dist p p -> p
expectation (Dist m) = L.foldl' (\b (a,p) -> p*a + b) 0 m

-- |Alias for @expectastion@.
mean :: (Fractional p) => Dist p p -> p
mean = expectation

-- |Compute the variance of a probability distribution.
variance :: (Fractional p) => Dist p p -> p
variance d@(Dist m) = L.foldl' (\b (a,p) -> b + p * (a-mean)^2) 0 m
    where
        mean = expectation d

-- |Compute the standard deviation of a probability distribution.
stdDev :: (Floating p) => Dist p p -> p
stdDev = sqrt . variance

-- |Compute the median of a probability distribution.
median :: (Fractional p, Ord p, Ord a) => Dist p a -> a
median (Dist m) = go 0 $ L.sortBy (comparing fst) m
    where
        go _ []         = error "Probabilities do not sum to 1.0 -- Control.Probability.Dist.MEDIAN"
        go p ((a,q):as) = if p + q >= 0.5
            then a
            else go (p+q) as

-- |Compute the mode of a probability distribution.
mode :: Ord p => Dist p a -> a
mode (Dist m) = fst $ L.maximumBy (comparing snd) m

-- |Compute the entropy of a distribution in a particular base. This only makes sens
--  for distributions where equal values can be grouped.
entropyBase :: (Floating p, Eq p, Ord a) => p -> Dist p a -> p
entropyBase k = negate . sum . map f . map snd . grouping . unD
    where
        f 0 = 0
        f p = p * logBase k p

-- |Compute the entropy of a probability distribution. This only makes sense for
--  distributions where equal values can be grouped. This computes the entropy in *nats* --
--  for more general units, see @entropyBase@.
entropy :: (Floating p, Eq p, Ord a) => Dist p a -> p
entropy = entropyBase (exp 1)

-- Utility function to group equal elements in a list, by converting the list
-- to a @Map@ and back.
grouping :: (Fractional p, Ord a) => [(a,p)] -> [(a,p)]
grouping = M.toList . M.fromListWith (+)



-- Class for computing expectations --

--class ToFloat a where
--    toFloat :: a -> Double

--instance ToFloat Integer where
--    toFloat = fromInteger

--instance ToFloat Int where
--    toFloat = fromIntegral

--instance ToFloat Float where
--    toFloat = float2Double

--instance 