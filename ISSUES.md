# Issues

A few issues with the library as it stands.

## Tracking filtered states for DistOrd

When using the 'DistOrd' instance it is missing some of the filtered cases when using 'condition'. This seems to be a non-observable bug, since when you compute realizable probabilities the filtered states don't matter, but it is still annoying!

## Slow computation for some trees.

Consider the following code

    bool = choose 0.5 True False :: Dist Bool

    alltrue = and <$> replicateM 20 bool

In theory, as soon as the tree branches once, we can examing the 'False' branch and realize that the 'and' predicate can never be satisfied there, and so assign a probability of 0.5 to that branch without computing further, so that we only end up computing the probability for ~20 individual branches to get a solution.

As it stands, what we actually do is expand each branch fully before summing the probabilities. Aside from the potential for floating point error, this is clearly inefficient as it ends up expanding the tree to ~1,000,000 elements before recombining them.

## Poor knowledge propagation

Consider the following two code samples

    bool1 = do
      a <- choose 0.5 True False
      return a

    bool2 = do
      a <- choose 0.5 True False
      returning a

Since we used the combinator 'choose', which tells the compiler that the elements have an 'Ord' instance, this knowledge should be propagated to the next statement that uses 'return', even though a use of 'return' does not require an 'Ord' instance. However, this does not happen at the moment:

    >>> bool1
    DistAny [(Just True, 0.5),(Just False,0.5)]
    >>> bool2
    DistOrd (fromList [(Just True,0.5),(Just False,0.5)])

It's not clear that it's possible to solve this, as the implementation of bind is something like

    Dist xs >>= f = collect [ f x | x <- toList xs ]

which can only use the efficient `DistOrd` constructor if at least one of the results of `f` is tagged with `DistOrd`. But if `f` is `return` we will automatically get the `DistAny` constructor.

## Violation of Monad Laws

One of the monad laws is

    m >>= return  =  m

but

    >>> let x = certainly () :: Dist ()
    >>> x
    DistOrd (fromList [(Just (),1.0)])
    >>> x >>= return
    DistAny [(Just (), 1.0)]

Instead, we only satisfy the following, weaker version of the Monad law:

    observe (x >>= return)  =  observe x

## Expectations

Should be possible to take the expectation of types that can't be cast to `Double`, `Float` or `Rational` (e.g. `Complex Double`) but I need to find the right abstraction! I think affine spaces are probably what's required.


## Efficiency of expectation, variance etc

Should improve the efficiency of these routines, writing one-pass algorithms where possible (and ensuring we don't get precision loss due to floating point - use Welford's algorithm?)


## Strict pairs

Instead of using pairs (a, p) in the probability list, do we want to use pairs that are (at least) strict in the second component, e.g. T !a !Double or T !a !p?



