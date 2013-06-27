## Probability

 A reasonably efficient probability monad, based on Oleg's [efficient Set monad](http://www.haskell.org/pipermail/haskell-cafe/2013-April/107607.html).

```haskell
>>> import Control.Probability
>>> let die = uniform [1..6]
>>> printProb $ sum (replicate 3 die)
 3  0.463 %
 4  1.389 %
 5  2.778 %
 6  4.630 %
 7  6.944 %
 8  9.722 %
 9 11.574 %
10 12.500 %
11 12.500 %
12 11.574 %
13  9.722 %
14  6.944 %
15  4.630 %
16  2.778 %
17  1.389 %
18  0.463 %
```