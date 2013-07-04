module Control.Probability.Ordering
    ( probLT
    , probLE
    , probGT
    , probGE
    , probEQ
    )
    where

import Control.Probability.Monad

-- |Returns a distribution giving the probability that one random variable is less than another.
probLT :: (Ord a, MonadProb m) => m a -> m a -> m Bool
probLT = liftP2 (<)

-- |Returns a distribution giving the probability that one random variable is less than or equal to another.
probLE :: (Ord a, MonadProb m) => m a -> m a -> m Bool
probLE = liftP2 (<=)

-- |Returns a distribution giving the probability that one random variable is greater than another.
probGT :: (Ord a, MonadProb m) => m a -> m a -> m Bool
probGT = liftP2 (>)

-- |Returns a distribution giving the probability that one random variable is greater than or equal to another.
probGE :: (Ord a, MonadProb m) => m a -> m a -> m Bool
probGE = liftP2 (>=)

-- |Returns a distribution giving the probability that one random variable is equal to another.
probEQ :: (Ord a, MonadProb m) => m a -> m a -> m Bool
probEQ = liftP2 (==)
