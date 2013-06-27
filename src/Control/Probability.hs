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
    ) where

import           Control.Probability.Internal
import           Control.Probability.Dist
import           Control.Probability.Ordering
import           Control.Probability.PrettyPrint










-- Playground

die :: (Ord a, Enum a, Num a) => a -> ProbMonad a
die n = uniform [1..n]

printProb x  = putStrLn . prettyPrint . runProb $ x

printProb' x = putStrLn . prettyPrint . runMostLikely $ x