{-| An efficient probability monad, taking inspiration from Oleg's /efficient
    Set monad/ at

        <http://www.haskell.org/pipermail/haskell-cafe/2013-April/107607.html>
    
    Most functions in the library have two versions -- a regular version, which
    carries an @Ord a@constraint, and a primed version, which has no constraints.
    In general you should use the unprimed versions, which are more efficient.
-}

module Control.Probability
    ( Dist
    , Distribution
    , Bayes
    , runProb
    , runProb'
    , runMostLikely
    , runMostLikely'
    -- Build distributions
    , select
    , sampleWithReplacement
    , sampleWithoutReplacement
    , unorderedSampleWithReplacement
    , unorderedSampleWithoutReplacement
    -- Compute statistics
    , expectation
    , variance
    , stdDev
    , mean
    , median
    , mode
    , entropyBase
    , entropy
    -- Modules
    , module Control.Monad
    , module Control.Applicative
    , module Control.Probability.MonadBayes
    , module Control.Probability.Ordering
    , module Control.Probability.PrettyPrint
    -- Playground
    , printProb
    , printProb'
    , printMostLikely
    , printMostLikely'
    ) where

import           Control.Monad
import           Control.Applicative
import           Data.List (delete)
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Text.Printf (PrintfArg)

import           Control.Probability.Types
import           Control.Probability.Bayes
import           Control.Probability.Dist
import           Control.Probability.Ordering
import           Control.Probability.PrettyPrint
import           Control.Probability.MonadBayes

type Distribution = Bayes Double

----------------------------------------------
-- Functions to build distributions
----------------------------------------------

selectSet :: (MonadBayes p m, Ord a) => Set.Set a -> m p (a, Set.Set a)
selectSet xs | Set.null xs = error "Empty list!"
selectSet xs = do
    x <- uniform $ Set.toList xs
    returning (x, Set.delete x xs)

select :: (MonadBayes p m, Ord a) => [a] -> m p (a, [a])
select [] = error "Empty list!"
select xs = do
    x <- uniform xs
    returning (x, delete x xs)

unRLE :: Map.Map a Int -> [a]
unRLE = go . Map.toList
    where
        go [] = []
        go ((x,n):xs) = replicate n x ++ go xs

unorderedSampleWithReplacement :: (MonadBayes p m, Ord a) => Int -> [a] -> m p [a]
unorderedSampleWithReplacement k set = liftP unRLE (go k)
    where
        go 0 = certainly Map.empty
        go n = do
            x  <- uniform set
            xs <- go (n - 1)
            returning (Map.insertWith (+) x 1 xs)

unorderedSampleWithoutReplacement :: (MonadBayes p m, Ord a) => Int -> [a] -> m p [a]
unorderedSampleWithoutReplacement k lst = liftP unRLE (go k lst)
    where

        go 0 set = certainly Map.empty
        go n set = do
            (x,rest) <- select set
            xs <- go (n-1) rest
            returning (Map.insertWith (+) x 1 xs)


sampleWithReplacement :: (MonadBayes p m, Ord a) => Int -> [a] -> m p [a]
sampleWithReplacement 0 set = certainly []
sampleWithReplacement k set = do
    x  <- uniform set
    xs <- sampleWithReplacement (k-1) set
    returning (x:xs)

sampleWithoutReplacement :: (MonadBayes p m, Ord a) => Int -> [a] -> m p [a]
sampleWithoutReplacement 0 set = certainly []
sampleWithoutReplacement k set = do
    (x, rest) <- select set
    xs <- sampleWithoutReplacement (k-1) rest
    returning (x:xs)