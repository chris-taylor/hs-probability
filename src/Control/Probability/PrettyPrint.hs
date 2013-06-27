module Control.Probability.PrettyPrint where

import qualified Data.List as L
import           Text.Printf
import           Control.Probability.Dist

-- |Pretty printer for probability distributions
prettyPrint :: Show a => Dist a -> String
prettyPrint (Dist m) = L.intercalate "\n" $ toString len str prb
    where
        str = map (show.fst) m
        prb = map (printf "%6.3f".(100*).snd) m
        len = maximum (map length str)
        toString n s p = map (\(s,p) -> padTo n s ++ " " ++ p ++ " %") (zip s p)
        padTo n s      = replicate (n-m) ' ' ++ s where m = length s