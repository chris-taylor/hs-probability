{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Probability.Internal
    ( ProbMonad(..)
    , runProb
    , runProb'
    , runMostLikely
    , runMostLikely'
    , norm
    , selectP
    )
    where

import qualified Data.List as L
import qualified Data.Map  as M
import           Data.Map (Map)
import           Data.Ord (comparing)
import           Data.Monoid
import           Control.Monad
import           Control.Applicative

import           Control.Probability.Dist
import           Control.Probability.Tree
import           Control.Probability.Monad

----------------------------------------------------------
-- Types
----------------------------------------------------------

-- |The probability monad type. The two constructors are for ordered and non-ordered types.
data ProbMonad p a where
    POrd :: Ord a => Map a p -> ProbMonad p a
    PAny ::          [(a,p)] -> ProbMonad p a



----------------------------------------------------------
-- Instances
----------------------------------------------------------

deriving instance (Show p, Show a) => Show (ProbMonad p a)

instance Fractional p => Functor (ProbMonad p) where
    fmap = liftM

instance Fractional p => Applicative (ProbMonad p) where
    pure  = return
    (<*>) = ap

instance Fractional p => Monad (ProbMonad p) where
    return  = certainly'
    m >>= f = collect . map (\(a,p) -> multProb p (f a)) $ toList m

instance (Fractional p, Ord a, Monoid a) => Monoid (ProbMonad p a) where
    mempty  = certainly mempty
    mappend = liftP2 mappend

instance (Fractional p, Ord a, Num a) => Num (ProbMonad p a) where
    (+) = liftP2 (+)
    (-) = liftP2 (-)
    (*) = liftP2 (*)
    fromInteger = certainly . fromInteger
    abs         = liftP abs
    signum      = liftP signum

instance (Fractional p, Ord a, Fractional a) => Fractional (ProbMonad p a) where
    (/)          = liftP2 (/)
    recip        = liftP  recip
    fromRational = certainly . fromRational

instance (Fractional p, Ord a, Floating a) => Floating (ProbMonad p a) where
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

instance (Fractional p) => MonadProb p ProbMonad where
    fromFreqs  = POrd . M.fromListWith (+)
    fromFreqs' = PAny


multProb :: (Fractional p) => p -> ProbMonad p a -> ProbMonad p a
multProb p (POrd x) = POrd $ M.map (*p) x
multProb p (PAny x) = PAny $ map (\(a, q) -> (a, p * q)) x

toList :: ProbMonad p a -> [(a,p)]
toList (POrd x) = M.toList x
toList (PAny x) = x

collect :: (Fractional p) => [ProbMonad p a] -> ProbMonad p a
collect [ ]        = PAny []
collect [x]        = x
collect (POrd x:t) = case collect t of
                        POrd y -> POrd (M.unionWith (+) x y)
                        PAny y -> POrd (M.unionWith (+) x (M.fromList y))
collect (PAny x:t) = case collect t of
                        POrd y -> POrd (M.unionWith (+) (M.fromList x) y)
                        PAny y -> PAny (x ++ y)



----------------------------------------------------------
-- User-Facing Methods
----------------------------------------------------------

-- |Convert a @ProbMonad a@ object into a probability distribution.
runProb :: (Fractional p, Ord a) => ProbMonad p a -> Dist p a
runProb (POrd x) = Dist (M.toList x)
runProb (PAny x) = Dist . grouping $ x

-- |Convert a @ProbMonad a@ object into a probability distribution, when there is
--  no @Ord@ instance for @a@.
runProb' :: ProbMonad p a -> Dist p a
runProb' (POrd x) = Dist (M.toList x)
runProb' (PAny x) = Dist (x)

-- |Convert a @ProbMonad a@ object into a probability distribution, ordering by
--  the most likely results. Must have an @Ord a@ instance.
runMostLikely :: (Fractional p, Ord p, Ord a) => ProbMonad p a -> Dist p a
runMostLikely (POrd x) = Dist . mostLikely . M.toList $ x
runMostLikely (PAny x) = Dist . mostLikely . grouping $ x

-- |Convert a @ProbMonad a@ object into a probability distribution, ordering by
--  the most likely results.
runMostLikely' :: (Fractional p, Ord p) => ProbMonad p a -> Dist p a
runMostLikely' (POrd x) = Dist . mostLikely $ M.toList x
runMostLikely' (PAny x) = Dist . mostLikely $ x

mostLikely :: (Fractional p, Ord p) => [(a,p)] -> [(a,p)]
mostLikely = L.sortBy . comparing $ negate . snd

-- |Normalize a @ProbMonad a@ object.
norm :: (Fractional p, Ord a) => ProbMonad p a -> ProbMonad p a
norm (POrd x) = POrd x
norm (PAny x) = POrd (M.fromListWith (+) x)

-- |Select a value from a probability distribution.
selectP :: (Fractional p, Ord p) => ProbMonad p a -> p -> a
selectP d p = go 0 (toList d)
    where
        go q ((a,r):as) = if q + r > p
            then a
            else go (q + r) as


