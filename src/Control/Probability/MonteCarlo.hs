{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Probability.MonteCarlo where


import qualified Control.Monad.Random as Random
import           Control.Applicative
import           Control.Monad (ap, liftM, liftM2, replicateM)
import           Data.Monoid
import           System.Random (StdGen)

import           Control.Probability.Types
import           Control.Probability.Class
import qualified Control.Probability.Distribution as Distribution
import           Control.Probability.Tree (mkTree, fetch)

newtype MonteCarlo p a = MonteCarlo { getMonteCarlo :: Random.Rand StdGen (Maybe a) }

-- | Extract a value from a Monte Carlo distribution using a random number
-- generator initialised using the system clock.
runMonteCarloIO :: MonteCarlo p a -> IO (Maybe a)
runMonteCarloIO = Random.evalRandIO . getMonteCarlo

-- | Extract a value from a Monte Carlo distribution using a supplied random
-- number generator.
runMonteCarlo :: StdGen -> MonteCarlo p a -> Maybe a
runMonteCarlo g t = Random.evalRand (getMonteCarlo t) g

instance Functor (MonteCarlo p) where
  fmap f = MonteCarlo . fmap (fmap f) . getMonteCarlo

instance Monad (MonteCarlo p) where

  return = MonteCarlo . return . Just

  mc >>= f = MonteCarlo $ do
    ma <- getMonteCarlo mc
    case ma of
      Nothing -> return Nothing
      Just a  -> getMonteCarlo (f a)

instance Applicative (MonteCarlo p) where
  pure  = return
  (<*>) = ap

instance Random.MonadRandom (MonteCarlo p) where

  getRandom  = MonteCarlo (fmap Just Random.getRandom)
  getRandoms = MonteCarlo (fmap Just Random.getRandoms)

  getRandomR  (a,b) = MonteCarlo (fmap Just $ Random.getRandomR  (a,b))
  getRandomRs (a,b) = MonteCarlo (fmap Just $ Random.getRandomRs (a,b))

--instance (Ord a, Monoid a) => Monoid (MonteCarlo p a) where
--    mempty  = return mempty
--    mappend = liftM2 mappend

instance (Ord a, Num a) => Num (MonteCarlo p a) where
  (+) = liftM2 (+)
  (-) = liftM2 (-)
  (*) = liftM2 (*)
  fromInteger = return . fromInteger
  abs         = liftM abs
  signum      = liftM signum

instance (Ord a, Fractional a) => Fractional (MonteCarlo p a) where
  (/)          = liftM2 (/)
  recip        = liftM  recip
  fromRational = return . fromRational

instance (Ord a, Floating a) => Floating (MonteCarlo p a) where
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

instance (Random.Random p, Probability p, Floating p) => MonadProb p MonteCarlo where

  fromWeights = fromWeights'
  
  fromWeights' xs = do
      let t = mkTree xs
      p <- Random.getRandomR (0,1)
      return $ fetch p t

  condition test = if test
      then return ()
      else MonteCarlo $ return Nothing

{-----------------------------------------------------------------
  Sample from a Monte Carlo generator.
-----------------------------------------------------------------}

-- | Sample a 'MonteCarlo' generator multiple times from a supplied random number
-- generator.
sampleMonteCarlo :: (Probability p, Ord a) => StdGen -> Int -> MonteCarlo p a -> ProbabilityList p a
sampleMonteCarlo g n mc  = fromResultList (sample g n mc)

-- | Sample a 'MonteCarlo' generator multiple times from a supplied random number generator.
sampleMonteCarlo' :: (Probability p) => StdGen -> Int -> MonteCarlo p a -> ProbabilityList p a
sampleMonteCarlo' g n mc = fromResultList' (sample g n mc)

-- | Sample from a 'MonteCarlo' multiple times, creating an 'IO ProbabilityList'.
sampleMonteCarloIO  :: (Probability p, Ord a) => Int -> MonteCarlo p a -> IO (ProbabilityList p a)
sampleMonteCarloIO n mc  = fromResultList  <$> sampleIO n mc

-- | Sample from a 'MonteCarlo' multiple times, creating an 'IO ProbabilityList'.
sampleMonteCarloIO' :: (Probability p) => Int -> MonteCarlo p a -> IO (ProbabilityList p a)
sampleMonteCarloIO' n mc = fromResultList' <$> sampleIO n mc

sample :: Probability p => StdGen -> Int -> MonteCarlo p a -> [Maybe a]
sample g n mc = Random.evalRand (replicateM n $ getMonteCarlo mc) g

sampleIO :: Probability p => Int -> MonteCarlo p a -> IO [Maybe a]
sampleIO n = Random.evalRandIO . replicateM n . getMonteCarlo

