
Make this less negative!

Currently it appears that in general the community who use numerical
methods, physicists, financial engineers, machine learning
practitioners, for example, are unaware of the benefits of using
strongly typed programming languages such as Haskell and in some cases
are even hostile ("Haskell $\ldots$ can be dismissed as an emphemeral
fad"). Moreover, although members of these communities would surely
never use a numerical method without being able to refer to
theoretical results demonstrating that the method produces a correct
answer they appear to be happy to use programming languages without
secure type theoretic foundations justifying the use of a particular
language on "pragmatic" grounds".

The audience is twofold:

Practitioners of numerical methods e.g. physicists who have put a toe
in the functional water but have developed a poor implementation and
come away with an impression that functional languages are impractical
for real work. For example, I saw a physicist implement an Ising model
using lists!!!!

Practitioners of functional languages who are unaware of the breadth
of practical applications of functional languages. For example,
implementing neural networks is significantly easier using automatic
differentiation in Haskell than using the traditional method of
backpropagation, solving partial differential equations using
computational stencils can be done just by writing down the stencil in
a very transparent way, etc. Now that I have written this, I think I
may need more compelling examples. How about regression using
automatic differentiation and also using Monte Carlo for a Bayesian
approach? I think that might be more accessible.