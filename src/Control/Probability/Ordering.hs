module Control.Probability.Ordering
    ( probLT
    , probLE
    , probGT
    , probGE
    , probEQ
    )
    where

import Control.Probability.MonadBayes

-- |Returns a distribution giving the probability that one random variable is less than another.
probLT :: (Ord a, MonadBayes p m) => m p a -> m p a -> m p Bool
probLT = liftP2 (<)

-- |Returns a distribution giving the probability that one random variable is less than or equal to another.
probLE :: (Ord a, MonadBayes p m) => m p a -> m p a -> m p Bool
probLE = liftP2 (<=)

-- |Returns a distribution giving the probability that one random variable is greater than another.
probGT :: (Ord a, MonadBayes p m) => m p a -> m p a -> m p Bool
probGT = liftP2 (>)

-- |Returns a distribution giving the probability that one random variable is greater than or equal to another.
probGE :: (Ord a, MonadBayes p m) => m p a -> m p a -> m p Bool
probGE = liftP2 (>=)

-- |Returns a distribution giving the probability that one random variable is equal to another.
probEQ :: (Ord a, MonadBayes p m) => m p a -> m p a -> m p Bool
probEQ = liftP2 (==)
