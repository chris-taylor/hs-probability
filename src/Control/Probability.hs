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
import           Text.Printf (PrintfArg)

import           Control.Probability.Types
import           Control.Probability.Bayes
import           Control.Probability.Dist
import           Control.Probability.Ordering
import           Control.Probability.PrettyPrint
import           Control.Probability.MonadBayes

type Distribution = Bayes Double

-- |Print a probability distribution to the screen (requires a @Ord@ instance).
printProb :: (Probability p, Ord a, Show a) => Bayes p a -> IO ()
printProb = putStrLn . prettyPrintGeneric . runProb

-- |Print a probability distribution to the screen.
printProb' :: (Probability p, Show a) => Bayes p a -> IO ()
printProb' = putStrLn . prettyPrintGeneric . runProb'

-- |Print a probability distribution to the screen, ordered by
--  likelihood (with the most probable elements first). Requires
--  an @Ord@ instance.
printMostLikely :: (Probability p, Ord a, Show a) => Bayes p a -> IO ()
printMostLikely = putStrLn . prettyPrintGeneric . runMostLikely

-- |Print a probability distribution to the screen, ordered by
--  likelihood (with the most probable elements first).
printMostLikely' :: (Probability p, Ord a, Show a) => Bayes p a -> IO ()
printMostLikely' = putStrLn . prettyPrintGeneric . runMostLikely'