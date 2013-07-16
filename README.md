## Probability

 A reasonably efficient probability monad, based on Oleg's [efficient Set monad](http://www.haskell.org/pipermail/haskell-cafe/2013-April/107607.html) and Martin Erwig's [PFP Library](http://web.engr.oregonstate.edu/~erwig/pfp/).

### Quick Tour

Start by importing the library

    >>> import Control.Probability

A die is a uniform distribution on the numbers 1 ... 6

    >>> let die = uniform [1..6] :: Distribution Int

The function `printProb` prints a representation of the distribution to the screen.

    >>> printProb die
    1  16.67%
    2  16.67%
    3  16.67%
    4  16.67%
    5  16.67%
    6  16.67%

What is the distribution of the sum of 3 dice?

    >>> printProb $ sum (replicate 3 die)
     3   0.46%
     4   1.39%
     5   2.78%
     6   4.63%
     7   6.94%
     8   9.72%
     9  11.57%
    10  12.50%
    11  12.50%
    12  11.57%
    13   9.72%
    14   6.94%
    15   4.63%
    16   2.78%
    17   1.39%
    18   0.46%

We can query distributions with the `??` operator. What is the probability that the sum of two dice is greater than 6?

    >>> printProb $ (>6) ?? die + die
    False  41.67%
     True  58.33%

What is the probability of two dice differing by one point?

    >>> printProb $ (==1) ?? abs (die - die)
    False  72.22%
     True  27.78%

Unlike the traditional probability monad based on lists, this library has no problem dealing with very large probability trees, e.g. those that result from rolling hundreds of dice.

    >>> printProb $ (>360) ?? sum (replicate 100 die)
    False  73.04%
     True  26.96%

More complicated functions can be built using the `Monad ProbMonad` instance and `do` notation. For example, if keep rolling a six-sided dice, what is the probability that the total hits exactly 10?

```haskell
untilEquals :: Int -> Distribution Bool
untilEquals n = go 0
    where
        go total | total >  n = certainly False
                 | total == n = certainly True
                 | otherwise  = do x <- die
                                   go (total + x)
```

and running it:

    >>> printProb $ untilEquals 10
    False  71.07%
     True  28.93%

### Bayesian Programming

Bayesian experiments can be run by using the function `condition` to filter out undesirable states. For example, consider the following problem:

> A family has two children. You are told that at least one of the children is a boy. What is the probability that the other child is also a boy?

The code to solve this problem is quite short:

```haskell
data Child = Boy | Girl deriving (Eq,Ord,Show)

dist :: Distribution Integer
dist = do
    a <- child
    b <- child
    let numberOfBoys = countBoys [a,b]
    condition (numberOfBoys > 0)
    returning (numberOfBoys)
  where
    child     = choose (1/2) Boy Girl
    countBoys = sum . filter (== Boy)
```

Running this results in

    >>> printProb dist -- distribution on total number of boys
    1  66.67%
    2  33.33% 

Which tells us that there is a 66.67% chance that there is only one boy, or a 33.3% chance that there are two boys. So the probability that the second child is also a boy is only 33.33%.

Or consider the following, more complicated problem:

> A jar contains 999 fair coins and 1 biased coin, which always comes up heads. A single coin is picked from the jar at random and tossed 10 times. If 10 heads in a row are observed, what is the probability that the next toss is also a head?

To solve this we can do

```haskell
data Coin = Head | Tail deriving (Eq,Ord,Show)

experiment :: Distribution Coin
experiment = do
    coin   <- pick
    tosses <- replicateM 10 coin
    condition (tosses == replicate 10 Head)
    nextToss <- coin
    return nextToss
  where
    fair   = choose (1/2) Head Tail
    biased = certainly Head
    pick   = choose (999/1000) fair biased
```

and running it:

    >>> printProb experiment
    Head  75.31%
    Tail  24.69%

