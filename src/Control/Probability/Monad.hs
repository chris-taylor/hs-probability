-- |This module contains the 'MonadProb' class and associated combinators. Each
--  method of 'MonadProb' comes in two flavors -- a primed and an unprimed
--  version. Implementations should ensure that they have identical behaviour
--  except that the unprimed version has an additional 'Ord' constraint (this
--  is to allow for efficient implementations which rely on being able to sort
--  the elements of the distribution.)

module Control.Probability.Monad
    ( MonadProb(..)
    , Prob
    , liftP
    , liftP2
    , liftP3
    , (??)
    ) where

import Control.Probability.Dist (Prob)

-- |A monad representing probability distributions. Minimal complete implementation
--  is 'fromFreqs' and 'fromFreqs\''.
class Monad m => MonadProb m where

    -- |Take a (not necessarily normalized) list of results and probabilities
    --  and output a probability distribution. This function requires an 'Ord' constraint.
    fromFreqs :: Ord a => [(a, Prob)] -> m a

    -- |Take a (not necessarily normalized) list of values and their frequences, and
    --  output a probability distribution. This function does not require an 'Ord' instance.
    fromFreqs' ::         [(a, Prob)] -> m a

    -- |Return a probability distribution with a certain result. Note that this is
    --  is equivalent to 'return' with an additional 'Ord' constraint, which is provided
    --  for efficiency.
    certainly :: Ord a => a -> m a
    certainly a = fromFreqs [(a, 1.0)]

    -- |Return a probability distribution with a certain result. Note that this is
    --  is equivalent to 'return' but is provided for efficiency and completeness.
    certainly' ::         a -> m a
    certainly' a = fromFreqs' [(a, 1.0)]

    -- |Return a uniform distribution over a (finite) list of values. This requires
    --  an 'Ord' constraint.
    uniform :: Ord a => [a] -> m a
    uniform xs = fromFreqs $ map (\a -> (a,p)) xs
        where
            p = 1 / fromIntegral (length xs)

    -- |Return a uniform distribution over a (finite) list of values. This does not require
    --  an 'Ord' constraint.
    uniform' ::         [a] -> m a
    uniform' xs = fromFreqs' $ map (\a -> (a,p)) xs
        where
            p = 1 / fromIntegral (length xs)

    -- |Returns a Bernoulli distribution that selects between two values -- selecting
    --  the first value with probability @p@ and the second with probability @1-p@. This
    --  requires an 'Ord' constraint.
    choose :: Ord a => Prob -> a -> a -> m a
    choose p a b = fromFreqs $ zip [a,b] [p,1-p]

    -- |Returns a Bernoulli distribution that selects between two values -- selecting
    --  the first value with probability @p@ and the second with probability @1-p@. This
    --  does not require an 'Ord' constraint.
    choose' ::         Prob -> a -> a -> m a
    choose' p a b = fromFreqs' $ zip [a,b] [p,1-p]



----------------------------------------------------------------
-- Lift functions into distributions
----------------------------------------------------------------

-- |Map a function over a distribution. This is equivalent to 'liftM' and 'fmap'
--  for 'Monad' and 'Functor' instances, except that it uses the 'certainly'
--  method from the 'MonadProb' class, and hence requires an 'Ord' instance in
--  the return type of @f@.
liftP  :: (Ord b, MonadProb m) => (a -> b) -> m a -> m b
liftP f p1 = p1 >>= certainly . f

-- |Map a function of two arguments over a distribution.
liftP2 :: (Ord c, MonadProb m) => (a -> b -> c) -> m a -> m b -> m c
liftP2 f p1 p2 = do x1 <- p1
                    x2 <- p2
                    certainly (f x1 x2)

-- |Map a function of three arguments over a distribution.
liftP3 :: (Ord d, MonadProb m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftP3 f p1 p2 p3 = do x1 <- p1
                       x2 <- p2
                       x3 <- p3
                       certainly (f x1 x2 x3)



----------------------------------------------------------------
-- Query a distribution
----------------------------------------------------------------

infixl 2 ??

-- |Query a probability distribution. For example,
--
-- >>> let die = uniform [1..6] :: ProbMonad Integer
-- >>> printProb $ (> 6) ?? die + die
-- False  41.667%
--  True  58.333%
(??) :: (MonadProb m) => (a -> Bool) -> m a -> m Bool
(??) = liftP
