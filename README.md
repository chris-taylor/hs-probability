## Probability

 A reasonably efficient probability monad, based on Oleg's [efficient Set monad](http://www.haskell.org/pipermail/haskell-cafe/2013-April/107607.html) and Martin Erwig's [PFP Library](http://web.engr.oregonstate.edu/~erwig/pfp/).

    >>> import Control.Probability

A die is a uniform distribution on the numbers 1 ... 6

    >>> let die = uniform [1..6]

The function `printProb` prints a representation of the distribution to the screen.

    >>> printProb die
    1  16.667%
    2  16.667%
    3  16.667%
    4  16.667%
    5  16.667%
    6  16.667%

What is the distribution of the sum of 3 dice?

    >>> printProb $ sum (replicate 3 die)
     3   0.463%
     4   1.389%
     5   2.778%
     6   4.630%
     7   6.944%
     8   9.722%
     9  11.574%
    10  12.500%
    11  12.500%
    12  11.574%
    13   9.722%
    14   6.944%
    15   4.630%
    16   2.778%
    17   1.389%
    18   0.463%

We can query distributions with the `??` operator. What is the probability that the sum of two dice is greater than 6?

    >>> printProb $ (>6) ?? die + die
    False  41.667%
     True  58.333%

What is the probability of two dice differing by one point?

    >>> printProb $ (==1) ?? abs (die - die)
    False  72.222%
     True  27.778%

Unlike the traditional probability monad based on lists, this library has no problem dealing with very large probability trees, e.g. those that result from rolling hundreds of dice.

    >>> printProb $ (>360) ?? sum (replicate 100 die)
    False  73.041%
     True  26.959%
