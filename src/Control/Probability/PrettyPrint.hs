-- | This module exports the function 'prettyPrintGeneric' which allows probability
-- distributions to be displayed to the screeen in a readable format. Each outcome
-- is displayed on a separate line, along with its corresponding probability.
-- For example,
--
-- >>> let die = uniform [1..6] :: Distribution Integer
-- >>> putStrLn . prettyPrintGeneric . runProb $ die
-- 1  16.67%
-- 2  16.67%
-- 3  16.67%
-- 4  16.67%
-- 5  16.67%
-- 6  16.67%
--
-- or
--
-- >>> let die = uniform [1..6] :: ProbMonad Rational Integer
-- >>> putStrLn . prettyPrintGeneric . runProb $ die
-- 1  1/6
-- 2  1/6
-- 3  1/6
-- 4  1/6
-- 5  1/6
-- 6  1/6
--
module Control.Probability.PrettyPrint
  ( ShowProb(..)
  , prettyPrintGeneric
  , printDist
  , printDist'
  , printMostLikely
  , printMostLikely'
  ) where

import qualified Data.List  as L
import           Control.Probability.Types
import           Control.Probability.Distribution

-- |Generic pretty printer for probability distributions. Each outcome is
--  displayed on a separate line, along with its corresponding probability.
prettyPrintGeneric :: (Show a, ShowProb p) => ProbabilityList p a -> [String]
prettyPrintGeneric m = genStrings (zip vals probs)
 where
  vals  = map (show . fst) m
  probs = map (disp . snd) m
  len   = maximum (map length vals)

  genStrings = map (\(s,p) -> padTo len s ++ "  " ++ p)
  padTo n s  = replicate (n-k) ' ' ++ s where k = length s

-- |Print a probability distribution to the screen (requires a @Ord@ instance).
printDist :: (Probability p, Ord a, Show a) => Distribution p a -> IO ()
printDist = mapM_ putStrLn . prettyPrintGeneric . runProb

-- |Print a probability distribution to the screen.
printDist' :: (Probability p, Show a) => Distribution p a -> IO ()
printDist' = mapM_ putStrLn . prettyPrintGeneric . runProb'

-- |Print a probability distribution to the screen, ordered by
--  likelihood (with the most probable elements first). Requires
--  an @Ord@ instance.
printMostLikely :: (Probability p, Ord a, Show a) => Distribution p a -> IO ()
printMostLikely = mapM_ putStrLn . prettyPrintGeneric . runMostLikely

-- |Print a probability distribution to the screen, ordered by
--  likelihood (with the most probable elements first).
printMostLikely' :: (Probability p, Ord a, Show a) => Distribution p a -> IO ()
printMostLikely' = mapM_ putStrLn . prettyPrintGeneric . runMostLikely'