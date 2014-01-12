# Issues

A few issues with the library as it stands.

## Tracking filtered states for DistOrd

When using the 'DistOrd' instance it is missing some of the filtered cases
when using 'condition'. This seems to be a non-observable bug, since when
you compute realizable probabilities the filtered states don't matter, but it
is still annoying!

## Slow computation for some trees.

Consider the following code

    bool = choose 0.5 True False :: Dist Bool

    alltrue = and <$> replicateM 20 bool

In theory, as soon as the tree branches once, we can examing the 'False' branch
and realize that the 'and' predicate can never be satisfied there, and so assign
a probability of 0.5 to that branch without computing further, so that we only
end up computing the probability for ~20 individual branches to get a solution.

As it stands, what we actually do is expand each branch fully before summing the
probabilities. Aside from the potential for floating point error, this is clearly
inefficient as it ends up expanding the tree to ~1,000,000 elements before recombining
them.

## Poor knowledge propagation

Consider the following two code samples

    bool1 = do
      a <- choose 0.5 True False
      return a

    bool2 = do
      a <- choose 0.5 True False
      returning a

Since we used the combinator 'choose', which tells the compiler that the elements
have an 'Ord' instance, this knowledge should be propagated to the next statement
that uses 'return', even though a use of 'return' does not require an 'Ord' instance.
However, this does not happen at the moment:

    >>> bool1
    DistAny [(Just True, 0.5),(Just False,0.5)]
    >>> bool2
    DistOrd (fromList [(Just True,0.5),(Just False,0.5)])


