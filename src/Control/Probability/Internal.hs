{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Probability.Internal
    ( ProbMonad(..)
    , runProb
    , runProb'
    , runMostLikely
    , runMostLikely'
    , norm
    , certainly
    , certainly'
    , bernoulli
    , bernoulli'
    , uniform
    , uniform'
    , fromFreqs
    , fromFreqs'
    , (??)
    , liftP
    , liftP2
    , liftP3
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

-- |The probability monad type. The two constructors are for ordered and non-ordered types.
data ProbMonad a where
    POrd :: Ord a => Map a Prob -> ProbMonad a
    PAny ::          [(a,Prob)] -> ProbMonad a

-- remove this eventually?
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

--  Utility function for implementing the Monad instance.
multProb :: Prob -> ProbMonad a -> ProbMonad a
multProb p (POrd x) = POrd $ M.map (*p) x
multProb p (PAny x) = PAny $ map (\(a,q) -> (a,p*q)) x

-- Utility function to convert to a list format.
toList :: ProbMonad a -> [(a,Prob)]
toList (POrd x) = M.toList x
toList (PAny x) = x

-- Utility function for implementing the monad instance - this is the 'secret sauce'
-- that makes it efficient.
collect :: [ProbMonad a] -> ProbMonad a
collect [ ]        = PAny []
collect [x]        = x
collect (POrd x:t) = case collect t of
                        POrd y -> POrd (M.unionWith (+) x y)
                        PAny y -> POrd (M.unionWith (+) x (M.fromList y))
collect (PAny x:t) = case collect t of
                        POrd y -> POrd (M.unionWith (+) (M.fromList x) y)
                        PAny y -> PAny (x ++ y)


-- ProbMonad (user-facing methods)


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

-- |Create a trivial probability distribution, that takes one value with certainty.
--  This functions works only with ordered types.
certainly :: Ord a => a -> ProbMonad a
certainly a = POrd $ M.singleton a 1.0

-- |Create a trivial probability distribution, that takes one value with certainty.
--  This function works with ordered and non-ordered types.
certainly' :: a -> ProbMonad a
certainly' a = PAny [(a, 1.0)]

-- |Create a Bernoulli distribution with probability @p@.
bernoulli :: Ord a => Prob -> a -> a -> ProbMonad a
bernoulli p a b = fromFreqs $ zip [a,b] [p,1-p]

-- |Create a Bernoulli distribution with probability @p@ for types
--  with no @Ord@ instance.
bernoulli' :: Prob -> a -> a -> ProbMonad a
bernoulli' p a b = fromFreqs' $ zip [a,b] [p,1-p]

-- |Create a uniform distribution for objects with an @Ord@ instance.
uniform :: Ord a => [a] -> ProbMonad a
uniform xs = POrd $ M.fromListWith (+) $ map (\x -> (x, p)) xs
    where
        p = 1.0 / fromIntegral (length xs)

-- |Create a uniform distribution for objects with no @Ord@ instance.
uniform' :: [a] -> ProbMonad a
uniform' xs = PAny $ map (\x -> (x, p)) xs
    where
        p = 1.0 / fromIntegral (length xs)

-- |Create a probability distribution from a frequency list.
fromFreqs :: Ord a => [(a,Prob)] -> ProbMonad a
fromFreqs = POrd . M.fromListWith (+)

-- |Create a probability distribution from a frequency list, for types without an @Ord@ instance.
fromFreqs' :: [(a,Prob)] -> ProbMonad a
fromFreqs' = PAny




infixl 2 ??

-- |Query a probability distribution.
(??) :: (a -> Bool) -> ProbMonad a -> ProbMonad Bool
(??) = liftP

-- |Map a function over a distribution.
liftP  :: (Ord b) => (a -> b) -> ProbMonad a -> ProbMonad b
liftP f p1 = p1 >>= certainly . f

-- |Map a function of two arguments over a distribution.
liftP2 :: Ord c => (a -> b -> c) -> ProbMonad a -> ProbMonad b -> ProbMonad c
liftP2 f p1 p2 = do x1 <- p1
                    x2 <- p2
                    certainly (f x1 x2)

-- |Map a function of three arguments over a distribution.
liftP3 :: Ord d => (a -> b -> c -> d) -> ProbMonad a -> ProbMonad b -> ProbMonad c -> ProbMonad d
liftP3 f p1 p2 p3 = do x1 <- p1
                       x2 <- p2
                       x3 <- p3
                       certainly (f x1 x2 x3)