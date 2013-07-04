{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

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
data ProbMonad a where
    POrd :: Ord a => Map a Prob -> ProbMonad a
    PAny ::          [(a,Prob)] -> ProbMonad a



----------------------------------------------------------
-- Instances
----------------------------------------------------------

deriving instance Show a => Show (ProbMonad a)

instance Functor ProbMonad where
    fmap = liftM

instance Applicative ProbMonad where
    pure  = return
    (<*>) = ap

instance Monad ProbMonad where
    return  = certainly'
    m >>= f = collect . map (\(a,p) -> multProb p (f a)) $ toList m

instance (Ord a, Monoid a) => Monoid (ProbMonad a) where
    mempty  = certainly mempty
    mappend = liftP2 mappend

instance (Ord a, Num a) => Num (ProbMonad a) where
    (+) = liftP2 (+)
    (-) = liftP2 (-)
    (*) = liftP2 (*)
    fromInteger = certainly . fromInteger
    abs         = liftP abs
    signum      = liftP signum

instance (Ord a, Fractional a) => Fractional (ProbMonad a) where
    (/)          = liftP2 (/)
    recip        = liftP  recip
    fromRational = certainly . fromRational

instance (Ord a, Floating a) => Floating (ProbMonad a) where
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

instance MonadProb ProbMonad where
    fromFreqs  = POrd . M.fromListWith (+)
    fromFreqs' = PAny


multProb :: Prob -> ProbMonad a -> ProbMonad a
multProb p (POrd x) = POrd $ M.map (*p) x
multProb p (PAny x) = PAny $ map (\(a,q) -> (a,p*q)) x

toList :: ProbMonad a -> [(a,Prob)]
toList (POrd x) = M.toList x
toList (PAny x) = x

collect :: [ProbMonad a] -> ProbMonad a
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
runProb :: Ord a => ProbMonad a -> Dist a
runProb (POrd x) = Dist (M.toList x)
runProb (PAny x) = Dist . grouping $ x

-- |Convert a @ProbMonad a@ object into a probability distribution, when there is
--  no @Ord@ instance for @a@.
runProb' :: ProbMonad a -> Dist a
runProb' (POrd x) = Dist (M.toList x)
runProb' (PAny x) = Dist (x)

-- |Convert a @ProbMonad a@ object into a probability distribution, ordering by
--  the most likely results. Must have an @Ord a@ instance.
runMostLikely :: Ord a => ProbMonad a -> Dist a
runMostLikely (POrd x) = Dist . mostLikely . M.toList $ x
runMostLikely (PAny x) = Dist . mostLikely . grouping $ x

-- |Convert a @ProbMonad a@ object into a probability distribution, ordering by
--  the most likely results.
runMostLikely' :: ProbMonad a -> Dist a
runMostLikely' (POrd x) = Dist . mostLikely $ M.toList x
runMostLikely' (PAny x) = Dist . mostLikely $ x

mostLikely :: [(a,Prob)] -> [(a,Prob)]
mostLikely = L.sortBy . comparing $ negate . snd

-- |Normalize a @ProbMonad a@ object.
norm :: Ord a => ProbMonad a -> ProbMonad a
norm (POrd x) = POrd x
norm (PAny x) = POrd (M.fromListWith (+) x)

-- |Select a value from a probability distribution.
selectP :: ProbMonad a -> Prob -> a
selectP d p = go 0 (toList d)
    where
        go q ((a,r):as) = if q + r > p
            then a
            else go (q + r) as


