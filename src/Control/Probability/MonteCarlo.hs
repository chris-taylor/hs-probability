{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Probability.MonteCarlo
    --( MC
    --, runMCIO
    --, runMC
    --)
    where


import qualified Control.Monad.Random as Random
import           Control.Applicative
import           Control.Monad (ap, liftM, liftM2)
import           Data.Monoid
import           System.Random (StdGen)

import           Control.Probability.Types
import           Control.Probability.Class
import qualified Control.Probability.Distribution as Distribution
import           Control.Probability.Tree (mkTree, fetch)

newtype MC p a = MC { getMC :: Random.Rand StdGen (Maybe a) }

-- |Extract a value from a Monte Carlo distribution using a random number
--  generator initialised using the system clock.
runMCIO :: MC p a -> IO (Maybe a)
runMCIO = Random.evalRandIO . getMC

-- |Extract a value from a Monte Carlo distribution using a supplied random
--  number generator.
runMC :: StdGen -> MC p a -> Maybe a
runMC g t = Random.evalRand (getMC t) g

instance Functor (MC p) where
    fmap f = MC . fmap (fmap f) . getMC

instance Monad (MC p) where

    return = MC . return . return

    MC r >>= f = MC $ r >>= \ma ->
        case ma of
            Nothing -> return Nothing
            Just a  -> getMC (f a)

instance Applicative (MC p) where
    pure  = return
    (<*>) = ap

-- |Monte Carlo generator that returns a value within a range.
randomR :: Random.Random a => (a,a) -> MC p a
randomR  = MC . fmap Just . Random.getRandomR

-- |Monte carlo generator that returns an infinite stream of values in a range.
randomRs :: Random.Random a => (a,a) -> MC p [a]
randomRs = MC . fmap Just . Random.getRandomRs

instance (Ord a, Monoid a) => Monoid (MC p a) where
    mempty  = return mempty
    mappend = liftM2 mappend

instance (Ord a, Num a) => Num (MC p a) where
    (+) = liftM2 (+)
    (-) = liftM2 (-)
    (*) = liftM2 (*)
    fromInteger = return . fromInteger
    abs         = liftM abs
    signum      = liftM signum

instance (Ord a, Fractional a) => Fractional (MC p a) where
    (/)          = liftM2 (/)
    recip        = liftM  recip
    fromRational = return . fromRational

instance (Ord a, Floating a) => Floating (MC p a) where
    pi      = return pi
    exp     = liftM exp
    sqrt    = liftM sqrt
    log     = liftM log
    (**)    = liftM2 (**)
    logBase = liftM2 logBase
    sin     = liftM sin
    tan     = liftM tan
    cos     = liftM cos
    asin    = liftM asin
    atan    = liftM atan
    acos    = liftM acos
    sinh    = liftM sinh
    tanh    = liftM tanh
    cosh    = liftM cosh
    asinh   = liftM asinh
    atanh   = liftM atanh
    acosh   = liftM acosh

instance (Random.Random p, Probability p, Floating p) => MonadProb p MC where

    fromWeights = fromWeights'
    
    fromWeights' xs = do
        let t = mkTree xs
        p <- randomR (0,1)
        return $ fetch p t

    condition test = if test
        then return ()
        else MC $ return Nothing

    certainly  = return
    certainly' = return



-----------------------------------------------------------------
---- Convert to and from Distribution
-----------------------------------------------------------------

---- |Sample from a 'MC' multiple times, creating an 'IO ProbabilityList'.
sampleMC  :: (Probability p, Ord a) => Int -> MC p a -> IO (ProbabilityList p a)
sampleMC n mc  = fromResultList  <$> sample n mc

---- |Sample from a 'MC' multiple times, creating an 'IO ProbabilityList'.
sampleMC' :: (Probability p) => Int -> MC p a -> IO (ProbabilityList p a)
sampleMC' n mc = fromResultList' <$> sample n mc

sample :: Probability p => Int -> MC p a -> IO [Maybe a]
sample n mc = Random.evalRandIO $ sequence $ replicate n $ getMC mc

