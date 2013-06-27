{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

{-| An efficient probability monad, taking inspiration from Oleg's /efficient
    Set monad/ at

        <http://www.haskell.org/pipermail/haskell-cafe/2013-April/107607.html>
    
    Most functions in the library have two versions -- a regular version, which
    carries an @Ord a@constraint, and a primed version, which has no constraints.
    In general you should use the unprimed versions, which are more efficient.
-}

module Control.Probability
    ( Prob
    , Dist
    , ProbMonad
    , runProb
    , runProb'
    , runMostLikely
    , runMostLikely'
    -- Create probability distributions
    , certainly
    , certainly'
    , uniform
    , uniform'
    , bernoulli
    , bernoulli'
    , fromFreqs
    , fromFreqs'
    -- Compute statistics
    , expectation
    , variance
    , stdDev
    , entropyBase
    , entropy
    -- Query distributions
    , (??)
    -- Lift to ProbMonad
    , liftP
    , liftP2
    , liftP3
    -- Ordering
    , probLT
    , probLE
    , probGT
    , probGE
    , probEQ
    -- Playground
    , printProb
    , printProb'
    , printMostLikely
    , printMostLikely'
    ) where

import           Control.Probability.Internal
import           Control.Probability.Dist
import           Control.Probability.Ordering
import           Control.Probability.PrettyPrint

-- |Print a probability distribution to the screen (requires a @Ord@ instance).
printProb :: (Ord a, Show a) => ProbMonad a -> IO ()
printProb = putStrLn . prettyPrint . runProb

-- |Print a probability distribution to the screen.
printProb' :: (Show a) => ProbMonad a -> IO ()
printProb' = putStrLn . prettyPrint . runProb'

-- |Print a probability distribution to the screen, ordered by
--  likelihood (with the most probable elements first). Requires
--  an @Ord@ instance.
printMostLikely :: (Ord a, Show a) => ProbMonad a -> IO ()
printMostLikely = putStrLn . prettyPrint . runMostLikely

-- |Print a probability distribution to the screen, ordered by
--  likelihood (with the most probable elements first).
printMostLikely' :: (Ord a, Show a) => ProbMonad a -> IO ()
printMostLikely' = putStrLn . prettyPrint . runMostLikely'