module Control.Probability.Ordering
    ( probLT
    , probLE
    , probGT
    , probGE
    , probEQ
    )
    where

import Control.Probability.Internal

-- |Returns a distribution giving the probability that one random variable is less than another.
probLT :: Ord a => ProbMonad a -> ProbMonad a -> ProbMonad Bool
probLT = liftP2 (<)

-- |Returns a distribution giving the probability that one random variable is less than or equal to another.
probLE :: Ord a => ProbMonad a -> ProbMonad a -> ProbMonad Bool
probLE = liftP2 (<=)

-- |Returns a distribution giving the probability that one random variable is greater than another.
probGT :: Ord a => ProbMonad a -> ProbMonad a -> ProbMonad Bool
probGT = liftP2 (>)

-- |Returns a distribution giving the probability that one random variable is greater than or equal to another.
probGE :: Ord a => ProbMonad a -> ProbMonad a -> ProbMonad Bool
probGE = liftP2 (>=)

-- |Returns a distribution giving the probability that one random variable is equal to another.
probEQ :: Ord a => ProbMonad a -> ProbMonad a -> ProbMonad Bool
probEQ = liftP2 (==)
