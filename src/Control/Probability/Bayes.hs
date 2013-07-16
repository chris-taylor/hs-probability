{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Probability.Bayes
    ( Bayes(..)
    , runProb
    , runProb'
    , runMostLikely
    , runMostLikely'
    )
    where

import qualified Data.List as L
import qualified Data.Map  as M
import           Data.Map (Map)
import           Data.Ord (comparing)
import           Data.Monoid
import           Control.Monad
import           Control.Applicative

import           Control.Probability.Types
import           Control.Probability.Tree
import           Control.Probability.MonadBayes

----------------------------------------------------------
-- Types
----------------------------------------------------------

-- |The probability monad type. The two constructors are for ordered and non-ordered types.
data Bayes p a where
    BayesOrd :: Ord a => Map (Maybe a) p -> Bayes p a
    BayesAny ::          [(Maybe a, p)]  -> Bayes p a

zeroDist  :: (Probability p, Ord a) => Bayes p a
zeroDist  = BayesOrd (M.singleton Nothing 1.0)

zeroDist' :: (Probability p)        => Bayes p a
zeroDist' = BayesAny [(Nothing,1.0)]

----------------------------------------------------------
-- Instances
----------------------------------------------------------

deriving instance (Show p, Show a) => Show (Bayes p a)

instance Probability p => Functor (Bayes p) where
    fmap = liftM

instance Probability p => Applicative (Bayes p) where
    pure  = return
    (<*>) = ap

instance Probability p => Monad (Bayes p) where
    return  = certainly'

    m >>= f = collect [multProb p (go a) | (a,p) <- toList' m]
        where
            go  Nothing = zeroDist'
            go (Just a) = f a

instance (Probability p, Ord a, Monoid a) => Monoid (Bayes p a) where
    mempty  = certainly mempty
    mappend = liftP2 mappend

instance (Probability p, Ord a, Num a) => Num (Bayes p a) where
    (+) = liftP2 (+)
    (-) = liftP2 (-)
    (*) = liftP2 (*)
    fromInteger = certainly . fromInteger
    abs         = liftP abs
    signum      = liftP signum

instance (Probability p, Ord a, Fractional a) => Fractional (Bayes p a) where
    (/)          = liftP2 (/)
    recip        = liftP  recip
    fromRational = certainly . fromRational

instance (Probability p, Ord a, Floating a) => Floating (Bayes p a) where
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

instance (Probability p) => MonadBayes p Bayes where
    fromWeights    = BayesOrd . M.fromListWith (+) . normalize
    fromWeights'   = BayesAny . normalize

    condition test = if test then certainly () else zeroDist

multProb :: (Probability p) => p -> Bayes p a -> Bayes p a
multProb p (BayesOrd x) = BayesOrd $ M.map (*p) x
multProb p (BayesAny x) = BayesAny [ (a,p * q) | (a,q) <- x ]

toList' :: Bayes p a -> [(Maybe a,p)]
toList' (BayesOrd x) = M.toList x
toList' (BayesAny x) = x

toList :: (Probability p, Ord a) => Bayes p a -> [(Maybe a,p)]
toList (BayesOrd x) = M.toList x
toList (BayesAny x) = grouping x

--fromList :: (Probability p, Ord a) => [(Maybe a,p)] -> Bayes p a
--fromList = BayesOrd . M.fromListWith (+) . normalize

--fromList' :: (Probability p) => [(Maybe a,p)] -> Bayes p a
--fromList' = BayesAny . normalize

collect :: (Probability p) => [Bayes p a] -> Bayes p a
collect [ ]        = BayesAny []
collect [x]        = x
collect (BayesOrd x:t) = case collect t of
                        BayesOrd y -> BayesOrd (M.unionWith (+) x y)
                        BayesAny y -> BayesOrd (M.unionWith (+) x (M.fromList y))
collect (BayesAny x:t) = case collect t of
                        BayesOrd y -> BayesOrd (M.unionWith (+) (M.fromList x) y)
                        BayesAny y -> BayesAny (x ++ y)

normalize :: (Probability p) => [(a,p)] -> [(Maybe a,p)]
normalize xs = map (\(a,p) -> (Just a, p/total)) xs
    where
        total = sum (map snd xs)

sumProbMaybe :: Probability p => [(Maybe a,p)] -> p
sumProbMaybe = L.foldl' f 0
    where
        f acc (x,p) = case x of
            Just _  -> acc + p
            Nothing -> acc

bayes :: Probability p => [(Maybe a,p)] -> [(a,p)]
bayes xs = foldr f [] xs
    where
        total       = sumProbMaybe xs
        f (x,p) acc = case x of
            Just a  -> (a,p/total) : acc
            Nothing -> acc


----------------------------------------------------------
-- User-Facing Methods
----------------------------------------------------------

-- |Convert a @Bayes a@ object into a probability distribution.
runProb :: (Probability p, Ord a) => Bayes p a -> Dist p a
runProb = Dist . bayes . toList

-- |Convert a @Bayes a@ object into a probability distribution, when there is
--  no @Ord@ instance for @a@.
runProb' :: Probability p => Bayes p a -> Dist p a
runProb' = Dist . bayes . toList'

-- |Convert a @Bayes a@ object into a probability distribution, ordering by
--  the most likely results. Must have an @Ord a@ instance.
runMostLikely :: (Probability p, Ord a) => Bayes p a -> Dist p a
runMostLikely = Dist . reorder . bayes . toList

-- |Convert a @Bayes a@ object into a probability distribution, ordering by
--  the most likely results.
runMostLikely' :: (Probability p) => Bayes p a -> Dist p a
runMostLikely' = Dist . reorder . bayes . toList'

reorder :: (Probability p) => [(a,p)] -> [(a,p)]
reorder = L.sortBy . comparing $ negate . snd

-- |Normalize a @Bayes a@ object.
--order :: (Probability p, Ord a) => Bayes p a -> Bayes p a
--order (BayesOrd x) = BayesOrd x
--order (BayesAny x) = BayesOrd (M.fromListWith (+) x)

-- |Select a value from a probability distribution.
--selectP :: (Probability p) => Bayes p a -> p -> a
--selectP d p = go 0 (toList d)
--    where
--        go q ((a,r):as) = if q + r > p
--            then a
--            else go (q + r) as


