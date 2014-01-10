{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Control.Probability.Types
    ( Probability
    , ProbabilityList
    , ShowProb(..)
    , grouping
    , fromResultList
    , fromResultList'
    ) where

import qualified Data.Maybe as Maybe
import qualified Data.Map   as M
import qualified Data.Ratio as R
import           Text.Printf (printf)

-----------------------------------------------------------------
-- The Probability class
-----------------------------------------------------------------

-- |A probability is any type that is both fractional and ordered.
class (Fractional p, Ord p, Eq p, ShowProb p) => Probability p where

instance Probability Double   where
instance Probability Float    where
instance Probability Rational where

-----------------------------------------------------------------
-- The Dist type
-----------------------------------------------------------------

-- |Probability distributions are just a wrapper around a list, pairing values
--  with probabilities.
type ProbabilityList p a = [(a,p)]

fromResultList  :: (Probability p, Ord a) => [Maybe a] -> ProbabilityList p a
fromResultList  = fromResultListGeneric grouping

fromResultList' :: (Probability p)        => [Maybe a] -> ProbabilityList p a
fromResultList' = fromResultListGeneric id

type ListTransformer a p = ProbabilityList p a -> ProbabilityList p a

fromResultListGeneric :: (Probability p) => ListTransformer a p -> [Maybe a] -> ProbabilityList p a
fromResultListGeneric collect xs = collect $ map (,p) ok
    where
        ok = Maybe.catMaybes xs
        p  = 1.0 / fromIntegral (length ok)

-----------------------------------------------------------------
-- ShowProb
-----------------------------------------------------------------

-- |Class for displaying probabilities. We don't want to use the 'Show' instance
--  as it has some unappealing properties (e.g. it always displays floats to full
--  precision, and rationals are displayed as @"2 % 3"@ rather than @"2/3"@).
class ShowProb p where
    disp :: p -> String

instance ShowProb Double where
    disp = showDoublePercent

instance ShowProb Float where
    disp = showFloatPercent

instance (Integral a, Show a) => ShowProb (R.Ratio a) where
    disp = showRational

showRational :: (Integral a, Show a) => R.Ratio a -> String
showRational r = show (R.numerator r) ++ "/" ++ show (R.denominator r)

showDouble :: Double -> String
showDouble = printf "%6.4f"

showDoublePercent :: Double -> String
showDoublePercent = printf "%6.2f%%" . (*100)

showFloat :: Float -> String
showFloat = printf "%6.4f"

showFloatPercent :: Float -> String
showFloatPercent = printf "%6.2f%%" . (*100)

-----------------------------------------------------------------
-- Utility functions
-----------------------------------------------------------------

-- Utility function to group equal elements in a list, by converting the list
-- to a @Map@ and back.
grouping :: (Probability p, Ord a) => ProbabilityList p a -> ProbabilityList p a
grouping = M.toList . M.fromListWith (+)
