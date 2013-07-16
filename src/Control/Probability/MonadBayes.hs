{-# LANGUAGE MultiParamTypeClasses #-}

-- |This module contains the 'MonadProb' class and associated combinators. Each
--  method of 'MonadProb' comes in two flavors -- a primed and an unprimed
--  version. Implementations should ensure that they have identical behaviour
--  except that the unprimed version has an additional 'Ord' constraint (this
--  is to allow for efficient implementations which rely on being able to sort
--  the elements of the distribution.)

module Control.Probability.MonadBayes
    ( MonadBayes(..)
    , returning
    , returning'
    , liftP
    , liftP2
    , liftP3
    , (??)
    ) where

-- |A monad representing probability distributions. Minimal complete implementation
--  is 'fromWeights' and 'fromWeights\''.
class (Fractional p, Monad (m p)) => MonadBayes p m where

    -- |Take a (not necessarily normalized) list of results and probabilities
    --  and output a probability distribution. This function requires an 'Ord' constraint.
    fromWeights  :: (Ord a) => [(a, p)] -> m p a

    -- |Take a (not necessarily normalized) list of values and their frequences, and
    --  output a probability distribution. This function does not require an 'Ord' instance.
    fromWeights' ::            [(a, p)] -> m p a

    -- |Assert that a condition is satisfied.
    condition :: Bool -> m p ()

    -- |Return a probability distribution with a certain result. Note that this is
    --  is equivalent to 'return' with an additional 'Ord' constraint, which is provided
    --  for efficiency.
    certainly :: Ord a => a -> m p a
    certainly a = fromWeights [(a, 1.0)]

    -- |Return a probability distribution with a certain result. Note that this is
    --  is equivalent to 'return' but is provided for efficiency and completeness.
    certainly' ::         a -> m p a
    certainly' a = fromWeights' [(a, 1.0)]

    -- |Return a uniform distribution over a (finite) list of values. This requires
    --  an 'Ord' constraint.
    uniform :: Ord a => [a] -> m p a
    uniform xs = fromWeights $ map (\a -> (a,p)) xs
        where
            p = 1 / fromIntegral (length xs)

    -- |Return a uniform distribution over a (finite) list of values. This does not require
    --  an 'Ord' constraint.
    uniform' ::         [a] -> m p a
    uniform' xs = fromWeights' $ map (\a -> (a,p)) xs
        where
            p = 1 / fromIntegral (length xs)

    -- |Returns a Bernoulli distribution that selects between two values -- selecting
    --  the first value with probability @p@ and the second with probability @1-p@. This
    --  requires an 'Ord' constraint.
    choose :: Ord a => p -> a -> a -> m p a
    choose p a b = fromWeights $ zip [a,b] [p,1-p]

    -- |Returns a Bernoulli distribution that selects between two values -- selecting
    --  the first value with probability @p@ and the second with probability @1-p@. This
    --  does not require an 'Ord' constraint.
    choose' ::         p -> a -> a -> m p a
    choose' p a b = fromWeights' $ zip [a,b] [p,1-p]

----------------------------------------------------------------
-- Synonyms
----------------------------------------------------------------

returning :: (MonadBayes p m, Ord a) => a -> m p a
returning = certainly

returning' :: (MonadBayes p m) => a -> m p a
returning' = certainly'

----------------------------------------------------------------
-- Lift functions into distributions
----------------------------------------------------------------

-- |Map a function over a distribution. This is equivalent to 'liftM' and 'fmap'
--  for 'Monad' and 'Functor' instances, except that it uses the 'certainly'
--  method from the 'MonadBayes' class, and hence requires an 'Ord' instance in
--  the return type of @f@.
liftP  :: (Ord b, MonadBayes p m) => (a -> b) -> m p a -> m p b
liftP f p1 = p1 >>= certainly . f

-- |Map a function of two arguments over a distribution.
liftP2 :: (Ord c, MonadBayes p m) => (a -> b -> c) -> m p a -> m p b -> m p c
liftP2 f p1 p2 = do x1 <- p1
                    x2 <- p2
                    certainly (f x1 x2)

-- |Map a function of three arguments over a distribution.
liftP3 :: (Ord d, MonadBayes p m) => (a -> b -> c -> d) -> m p a -> m p b -> m p c -> m p d
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
-- >>> let die = uniform [1..6] :: Distribution Integer
-- >>> printProb $ (> 6) ?? die + die
-- False  41.667%
--  True  58.333%
(??) :: (MonadBayes p m) => (a -> Bool) -> m p a -> m p Bool
(??) = liftP

