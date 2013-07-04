{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Probability.Random
    --( T(..)
    --, run
    --, runSeed
    --)
    where


import qualified Control.Monad.Random as Random
import           Control.Applicative
import           Data.Monoid
import           System.Random (StdGen)

import qualified Control.Probability.Internal as Internal
import           Control.Probability.Dist (Prob)
import           Control.Probability.Tree (mkTree, fetch)
import           Control.Probability.Monad

newtype T a = T { unT :: Random.Rand StdGen a } deriving (Functor,Monad,Applicative)

-- |Extract a value from a Monte Carlo distribution using a random number
--  generator initialised using the system clock.
run :: T a -> IO a
run = Random.evalRandIO . unT

-- |Extract a value from a Monte Carlo distribution using a supplied random
--  number generator.
runSeed :: StdGen -> T a -> a
runSeed g t = Random.evalRand (unT t) g

-- |Monte Carlo generator that returns a value within a range.
randomR :: Random.Random a => (a,a) -> T a
randomR = T . Random.getRandomR

-- |Monte carlo generator that returns an infinite stream of values in a range.
randomRs :: Random.Random a => (a,a) -> T [a]
randomRs = T . Random.getRandomRs

liftT :: (a -> b) -> T a -> T b
liftT f = T . fmap f . unT

liftT2 :: (a -> b -> c) -> T a -> T b -> T c
liftT2 f t1 t2 = do x1 <- t1
                    x2 <- t2
                    return $ f x1 x2

instance (Ord a, Monoid a) => Monoid (T a) where
    mempty  = certainly mempty
    mappend = liftT2 mappend

instance (Ord a, Num a) => Num (T a) where
    (+) = liftT2 (+)
    (-) = liftT2 (-)
    (*) = liftT2 (*)
    fromInteger = certainly . fromInteger
    abs         = liftT abs
    signum      = liftT signum

instance (Ord a, Fractional a) => Fractional (T a) where
    (/)          = liftT2 (/)
    recip        = liftT  recip
    fromRational = certainly . fromRational

instance (Ord a, Floating a) => Floating (T a) where
    pi      = certainly pi
    exp     = liftT exp
    sqrt    = liftT sqrt
    log     = liftT log
    (**)    = liftT2 (**)
    logBase = liftT2 logBase
    sin     = liftT sin
    tan     = liftT tan
    cos     = liftT cos
    asin    = liftT asin
    atan    = liftT atan
    acos    = liftT acos
    sinh    = liftT sinh
    tanh    = liftT tanh
    cosh    = liftT cosh
    asinh   = liftT asinh
    atanh   = liftT atanh
    acosh   = liftT acosh

instance MonadProb T where
    fromFreqs' xs = do
        let t = mkTree xs
        p <- randomR (0,1)
        return (fetch p t)
    fromFreqs = fromFreqs'
    certainly  = return
    certainly' = return





---------------------------------------------------------------
-- Convert to and from ProbMonad
---------------------------------------------------------------

type Distribution a = T (Internal.ProbMonad a)

-- |Convert from a @ProbMonad@ into a @T@.
pick :: Internal.ProbMonad a -> T a
pick d = Internal.selectP d <$> randomR (0,1)

-- |Convert from a list of @T@ into a @ProbMonad@ (with @Ord@ instance)
dist :: Ord a => [T a] -> Distribution a
dist = fmap uniform . sequence

-- |Convert from a list of @T@ into a @ProbMonad@ (no @Ord@ instance required)
dist' ::         [T a] -> Distribution a
dist' = fmap uniform' . sequence

-- |Sample from a @T@ multiple times, creating a @ProbMonad@.
sample :: Ord a => Int -> T a -> Distribution a
sample n = dist . replicate n

-- |Sample from a @T@ multiple times, creating a @ProbMonad@.
sample' ::         Int -> T a -> Distribution a
sample' n = dist' . replicate n

