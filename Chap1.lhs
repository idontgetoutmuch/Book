% Laplace's Equation: Haskell and C
% Dominic Steinitz
% 15th January 2014

---
bibliography: Book.bib
---

\usepackage{color}

```{.dia height='300'}
import Diagram
dia = (fivePointStencil # scaleX 0.4 # scaleY 0.4) <>
      (ninePointStencil # scaleX 0.4 # scaleY 0.4 # translate (r2 (0.5, 0.0)))
```


Introduction
============

Suppose we have a square thin plate of metal and we hold each of edges
at a temperature which may vary along the edge but is fixed for all
time. After some period depending on the conductivity of the metal,
the temperature at every point on the plate will have stabilised. What
is the temperature at any point?

We can calculate this using by solving Laplace's equation $\nabla^2
\phi = 0 $ in 2 dimensions. Apart from the preceeding motivation, a
more compelling reason for doing so is that it is a moderately simple
equation, in so far as partial differential equations are simple, that
has been well studied for centuries.

In Haskell terms this gives us the opportunity to use the
[repa](http://hackage.haskell.org/package/repa) library and use
[hmatrix](http://hackage.haskell.org/package/hmatrix) which is based
on [Lapack](http://www.netlib.org/lapack/) (as well as other
libraries) albeit hmatrix only for illustratative purposes.

I had originally intended this blog to contain a comparison repa's
performance against an equivalent C program even though this has
already been undertaken by the repa team in their various
publications. And indeed it is still my intention to produce such a
comparision. However, as I investigated further, it turned out a fair
amount of comparison work has already been done by a
[team](http://www.cs.ru.nl/P.Achten/IFL2013/symposium_proceedings_IFL2013/ifl2013_submission_20.pdf) from Intel which suggests there is currently a performance gap but one which is not so large that it outweighs the other benefits of Haskell.

To be more specific, one way in which using repa stands out from the
equivalent C implementation is that it gives a language in which we
can specify the
[stencil](http://en.wikipedia.org/wiki/Stencil_%28numerical_analysis%29)
being used to solve the equation. As an illustration we substitute the
[nine
point](http://www.physics.arizona.edu/~restrepo/475B/Notes/sourcehtml/node52.html#fg.nin2)
method for the [five
point](http://en.wikipedia.org/wiki/Five-point_stencil) method merely
by changing the stencil.

A Motivating Example: The Steady State Heat Equation
----------------------------------------------------

[Fourier's
law](http://en.wikipedia.org/wiki/Thermal_conduction#Fourier.27s_law)
states that the rate of heat transfer or the flux
$\boldsymbol{\sigma}$ is proportional to the negative temperature
gradient, as heat flows from hot to cold, and further that it flows in
the direction of greatest temperature change. We can write this as

$$
\boldsymbol{\sigma} = -k\nabla \phi
$$

where $\phi : \mathbb{R} \times \mathbb{R} \rightarrow \mathbb{R}$ is
the temperature at any given point on the plate and $k$ is the
conductivity of the metal.

Moreover, we know that for any region on the plate, the total amount
of heat flowing in must be balanced by the amount of heat flowing
out. We can write this as

$$
\nabla \cdot \boldsymbol{\sigma} = 0
$$

Substituting the first equation into the second we obtain Laplace's equation

$$
\nabla^2 \phi = 0
$$

For example, suppose we hold the temperature of the edges of the plate
as follows

$$
\begin{matrix}
\phi(x, 0) = 1 & \phi(x, 1) = 2 & \phi(0, y) = 1 & \phi(1, y) = 2
\end{matrix}
$$

then after some time the temperature of the plate will be as shown in
the heatmap below.

```{.dia width='600'}
dia = image "diagrams/heatmap.png" 1.5 1.0
```

Notes:

 1. Red is hot.

 2. Blue is cold.

 3. The heatmap is created by a finite difference method described
below.

 4. The $y$-axis points down (not up) i.e. $\phi(x,1)$ is at the
bottom, reflecting the fact that we are using an array in the finite
difference method and rows go down not up.

 5. The corners are grey because in the five point finite difference
method these play no part in determining temperatures in the interior
of the plate.


Colophon
========

Since the book I am writing contains C code (for performance
comparisons), I need a way of being able to compile and run this code
and include it "as is" in the book. Up until now, all my blog posts
have contained Haskell and so I have been able to use
[BlogLiteratelyD](http://hackage.haskell.org/package/BlogLiterately-diagrams)
which allows me to include really nice
[diagrams](http://projects.haskell.org/diagrams/gallery.html). But
clearly this tool wasn't really designed to handle other languages
(although I am sure it could be made to do so).

Using pandoc's
[scripting](http://johnmacfarlane.net/pandoc/scripting.html)
capability with the small script provided

~~~~{.haskell include="Include.hs"}
~~~~

I can then include C code blocks like this

    ~~~~ {.c include="Chap1a.c"}
    ~~~~

And format the whole document like this

~~~~ {#workflow}
pandoc -s Chap1.lhs --filter=./Include -t markdown+lhs > Chap1Expanded.lhs
BlogLiteratelyD Chap1Expanded.lhs > Chap1.html
~~~~

Sadly, the C doesn't get syntax highlighting but this will do for now.

Acknowledgements
================

A lot of the code for this post is taken from the
[repa](http://repa.ouroborus.net) package itself. Many thanks to the
repa team for providing the package and the example code.

Haskell Preamble
================

> {-# OPTIONS_GHC -Wall                      #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults    #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
> {-# OPTIONS_GHC -fno-warn-missing-methods  #-}
> {-# OPTIONS_GHC -fno-warn-orphans          #-}

> {-# LANGUAGE BangPatterns                  #-}
> {-# LANGUAGE TemplateHaskell               #-}
> {-# LANGUAGE QuasiQuotes                   #-}
> {-# LANGUAGE NoMonomorphismRestriction     #-}

> module Chap1 (
>     solveLaplaceStencil
>   , useBool
>   , boundMask
>   , boundValue
>   , bndFnEg1
>   , fivePoint
>   , ninePoint
>   , testStencil5
>   , testStencil9
>   , analyticValue
>   , slnHMat4
>   , slnHMat5
>   , testJacobi4
>   , testJacobi6
>   , bndFnEg3
>   , runSolver
>   ) where
>
> import Data.Array.Repa                   as R
> import Data.Array.Repa.Unsafe            as R
> import Data.Array.Repa.Stencil           as A
> import Data.Array.Repa.Stencil.Dim2      as A

> import Prelude                           as P

> import Data.Packed.Matrix
> import Numeric.LinearAlgebra.Algorithms

> import Text.PrettyPrint.HughesPJClass ( render )

> import Chap1Aux

> import Control.Applicative

Laplace's Equation: The Five Point Formula
==========================================

We show how to apply finite difference methods to [Laplace's](Laplace) equation:

  [Laplace]: http://en.wikipedia.org/wiki/Laplace's_equation

$$
\nabla^2 u = 0
$$

where

$$
\nabla^2 = \frac{\partial^2}{\partial x^2} +\frac{\partial^2}{\partial y^2}
$$

For a sufficiently smooth function (see [@iserles2009first Chapter 8]) we have

$$
\begin{aligned}
\frac{\partial^2 u}{\partial x^2}\mathop{\Bigg|_{x = x_0 + k\Delta x}}_{y = y_0 + l\Delta x} &= \frac{1}{(\Delta x)^2}\Delta_{0,x}^2 u_{k,l} + \mathcal{O}((\Delta x)^2) \\
\frac{\partial^2 u}{\partial y^2}\mathop{\Bigg|_{x = x_0 + k\Delta x}}_{y = y_0 + l\Delta x} &= \frac{1}{(\Delta x)^2}\Delta_{0,y}^2 u_{k,l} + \mathcal{O}((\Delta x)^2)
\end{aligned}
$$

where the central difference operator $\Delta_0$ is defined as

$$
(\Delta_0 z)_k \triangleq z_{k + \frac{1}{2}} - z_{k - \frac{1}{2}}
$$

We are therefore led to consider the *five point* difference scheme.

$$
\frac{1}{(\Delta x)^2}(\Delta_{0,x}^2 + \Delta_{0,y}^2) u_{k,l} = 0
$$

We can re-write this explicitly as

$$
u_{k-1,l} + u_{k+1,l} + u_{k,l-1} + u_{k,l+1} - 4u_{k,l} = 0
$$

Specifically for the grid point (2,1) in a $4 \times 4$ grid we have

$$
{\color{blue}{u_{1,1}}} + {\color{red}{u_{3,1}}} + {\color{red}{u_{2,0}}} + {\color{blue}{u_{2,2}}} - 4{\color{blue}{u_{2,1}}} = 0
$$

where blue indicates that the point is an interior point and red
indicates that the point is a boundary point. For Dirichlet boundary
conditions (which is all we consider in this post), the values at the
boundary points are known.

```{.dia height='500'}
import Diagram
dia = example 3 3
```

We can write the entire set of equations for this grid as

~~~~ {#verbatim include="matrix3.tex"}
~~~~

A Very Simple Example
---------------------

Let us take the boundary conditions to be

$$
\begin{matrix}
u(x, 0) = 1 & u(x, 1) = 2 & u(0, y) = 1 & u(1, y) = 2
\end{matrix}
$$

With our $4 \times 4$ grid we can solve this exactly using the
[hmatrix](http://hackage.haskell.org/package/hmatrix) package which
has a binding to [LAPACK](http://www.netlib.org/lapack/).

First we create a $4 \times 4$ matrix in *hmatrix* form

> simpleEgN :: Int
> simpleEgN = 4 - 1
>
> matHMat4 :: IO (Matrix Double)
> matHMat4 = do
>   matRepa <- computeP $ mkJacobiMat simpleEgN :: IO (Array U DIM2 Double)
>   return $ (simpleEgN - 1) >< (simpleEgN - 1) $ toList matRepa

    [ghci]
    matHMat4

Next we create the column vector as presribed by the boundary conditions

> bndFnEg1 :: Int -> Int -> (Int, Int) -> Double
> bndFnEg1 _ m (0, j) |           j > 0 && j < m = 1.0
> bndFnEg1 n m (i, j) | i == n && j > 0 && j < m = 2.0
> bndFnEg1 n _ (i, 0) |           i > 0 && i < n = 1.0
> bndFnEg1 n m (i, j) | j == m && i > 0 && i < n = 2.0
> bndFnEg1 _ _ _                                 = 0.0

> bnd1 :: Int -> [(Int, Int)] -> Double
> bnd1 n = negate .
>          sum .
>          P.map (bndFnEg1 n n)

> bndHMat4 :: Matrix Double
> bndHMat4 = ((simpleEgN - 1) * (simpleEgN - 1)) >< 1 $
>            mkJacobiBnd fromIntegral bnd1 3

    [ghci]
     bndHMat4

> slnHMat4 :: IO (Matrix Double)
> slnHMat4 = matHMat4 >>= return . flip linearSolve bndHMat4

    [ghci]
    slnHMat4

The Jacobi Method
=================

Inverting a matrix is expensive so instead we use the (possibly most)
classical of all iterative methods, Jacobi iteration. Given
$A\boldsymbol{x} = \boldsymbol{b}$ and an estimated solution
$\boldsymbol{x}_i^{[k]}$, we can generate an improved estimate
$\boldsymbol{x}_i^{[k+1]}$. See [@iserles2009first Chapter 12] for the
details on convergence and convergence rates.

$$
\boldsymbol{x}_i^{[k+1]} = \frac{1}{A_{i,i}}\Bigg[\boldsymbol{b}_i - \sum_{j \neq i} A_{i,j}\boldsymbol{x}_j^{[k]}\Bigg]
$$

The simple example above does not really give a clear picture of what
happens in general during the update of the estimate. Here is a larger
example

```{.dia height='500'}
import Diagram
dia = example 5 5
```

~~~~ {#verbatim include="matrix5.tex"}
~~~~

Expanding the matrix equation for a $\color{blue}{\text{point}}$ *not*
in the $\color{red}{\text{boundary}}$ we get

$$
x_{i,j}^{[k+1]} = \frac{1}{4}(x^{[k]}_{i-1,j} + x^{[k]}_{i,j-1} + x^{[k]}_{i+1,j} + x^{[k]}_{i,j+1})
$$

Cleary the values of the points in the boundary are fixed and must
remain at those values for every iteration.

Here is the method using *repa*. To produce an improved estimate, we
define a function *relaxLaplace* and we pass in a *repa* matrix
representing our original estimate $\boldsymbol{x}_i^{[k]}$ and
receive the one step update $\boldsymbol{x}_i^{[k+1]}$ also as a
*repa* matrix.

We pass in a boundary condition mask which specifies which points are
boundary points; a point is a boundary point if its value is 1.0 and
not if its value is 0.0.

> boundMask :: Monad m => Int -> Int -> m (Array U DIM2 Double)
> boundMask gridSizeX gridSizeY = computeP $
>                                 fromFunction (Z :. gridSizeX + 1 :. gridSizeY + 1) f
>   where
>     f (Z :. _ix :.  iy) | iy == 0         = 0
>     f (Z :. _ix :.  iy) | iy == gridSizeY = 0
>     f (Z :.  ix :. _iy) | ix == 0         = 0
>     f (Z :.  ix :. _iy) | ix == gridSizeX = 0
>     f _                                   = 1

Better would be to use at least a *Bool* as the example below shows
but we wish to modify the code from the [repa git
repo](https://github.com/DDCSF/repa) as little as possible.


> useBool :: IO (Array U DIM1 Double)
> useBool = computeP $
>           R.map (fromIntegral . fromEnum) $
>           fromFunction (Z :. (3 :: Int)) (const True)

    [ghci]
    useBool

We further pass in the boundary conditions. We construct these by
using a function which takes the grid size in the $x$ direction,
the grid size in the $y$ direction and a given pair of co-ordinates in
the grid and returns a value at this position.

> boundValue :: Monad m =>
>               Int ->
>               Int ->
>               (Int -> Int -> (Int, Int) -> Double) ->
>               m (Array U DIM2 Double)
> boundValue gridSizeX gridSizeY bndFn =
>   computeP $
>   fromFunction (Z :. gridSizeX + 1 :. gridSizeY + 1) g
>   where
>     g (Z :. ix :. iy) = bndFn gridSizeX gridSizeY (ix, iy)

Note that we only update an element in the *repa* matrix
representation of the vector if it is **not** on the boundary.

> relaxLaplace
>   :: Monad m
>      => Array U DIM2 Double
>      -> Array U DIM2 Double
>      -> Array U DIM2 Double
>      -> m (Array U DIM2 Double)
>
> relaxLaplace arrBoundMask arrBoundValue arr
>   = computeP
>     $ R.zipWith (+) arrBoundValue
>     $ R.zipWith (*) arrBoundMask
>     $ unsafeTraverse arr id elemFn
>   where
>     _ :. height :. width
>       = extent arr
>
>     elemFn !get !d@(sh :. i :. j)
>       = if isBorder i j
>         then  get d
>         else (get (sh :. (i-1) :. j)
>               +   get (sh :. i     :. (j-1))
>               +   get (sh :. (i+1) :. j)
>               +   get (sh :. i     :. (j+1))) / 4
>     isBorder !i !j
>       =  (i == 0) || (i >= width  - 1)
>          || (j == 0) || (j >= height - 1)

We can use this to iterate as many times as we like.

> solveLaplace
> 	:: Monad m
>         => Int
>         -> Array U DIM2 Double
>         -> Array U DIM2 Double
>         -> Array U DIM2 Double
>         -> m (Array U DIM2 Double)
>
> solveLaplace steps arrBoundMask arrBoundValue arrInit
>  = go steps arrInit
>   where
>     go !i !arr
>       | i == 0
>       = return     arr
>
>       | otherwise
>       = do arr' <- relaxLaplace arrBoundMask arrBoundValue arr
>            go (i - 1) arr'

For our small example, we set the initial array to $0$ at every
point. Note that the function which updates the grid, *relaxLaplace*
will immediately over-write the points on the boundary with values
given by the boundary condition.

> mkInitArrM :: Monad m => Int -> m (Array U DIM2 Double)
> mkInitArrM n = computeP $ fromFunction (Z :. (n + 1) :. (n + 1)) (const 0.0)

We can now test the Jacobi method

> testJacobi4 :: Int -> IO (Array U DIM2 Double)
> testJacobi4 nIter = do
>   mask    <- boundMask simpleEgN simpleEgN
>   val     <- boundValue simpleEgN simpleEgN bndFnEg1
>   initArr <- mkInitArrM simpleEgN
>   solveLaplace nIter mask val initArr

After 55 iterations, we obtain convergence up to the limit of accuracy
of double precision floating point numbers. Note this only provides a
solution of the matrix equation which is an approximation to Laplace's
equation. To obtain a more accurate result for the latter we need to
use a smaller grid size.

    [ghci]
    testJacobi4 55 >>= return . pPrint

A Larger Example
----------------

Armed with Jacobi, let us now solve a large example.

> largerEgN, largerEgN2 :: Int
> largerEgN = 6 - 1
> largerEgN2 = (largerEgN - 1) * (largerEgN - 1)

First let us use *hmatrix*.

> matHMat5 :: IO (Matrix Double)
> matHMat5 = do
>   matRepa <- computeP $ mkJacobiMat largerEgN :: IO (Array U DIM2 Double)
>   return $ largerEgN2 >< largerEgN2 $ toList matRepa

    [ghci]
    matHMat5

> bndHMat5 :: Matrix Double
> bndHMat5 = largerEgN2>< 1 $ mkJacobiBnd fromIntegral bnd1 5

    [ghci]
     bndHMat5

> slnHMat5 :: IO (Matrix Double)
> slnHMat5 = matHMat5 >>= return . flip linearSolve bndHMat5

    [ghci]
    slnHMat5

And for comparison, let us use the Jacobi method.

> testJacobi6 :: Int -> IO (Array U DIM2 Double)
> testJacobi6 nIter = do
>   mask    <- boundMask largerEgN largerEgN
>   val     <- boundValue largerEgN largerEgN bndFnEg1
>   initArr <- mkInitArrM largerEgN
>   solveLaplace nIter mask val initArr

    [ghci]
    testJacobi6 178 >>= return . pPrint

Note that with a larger grid we need more points (178) before the
Jacobi method converges.

Stencils
========

Since we are functional programmers, our natural inclination is to see
if we can find an abstraction for (at least some) numerical
methods. We notice that we are updating each grid element (except the
boundary elements) by taking the North, East, South and West
surrounding squares and calculating a linear combination of these.

```{.dia height='300'}
import Diagram
dia = fivePointStencil
```

Repa provides this abstraction and we can describe the update
calculation as a
[stencil](http://en.wikipedia.org/wiki/Stencil_%28numerical_analysis%29). [@Lippmeier:2011:EPS:2034675.2034684]
gives full details of stencils in repa.

> fivePoint :: Stencil DIM2 Double
> fivePoint = [stencil2|  0 1 0
>                         1 0 1
>                         0 1 0 |]

Using stencils allows us to modify our numerical method with a very
simple change. For example, suppose we wish to use the nine point
method (which is $\mathcal{O}((\Delta x)^4)$!) then we only need write
down the stencil for it which is additionally a linear combination of
North West, North East, South East and South West.

```{.dia height='300'}
import Diagram
dia = ninePointStencil
```

> ninePoint :: Stencil DIM2 Double
> ninePoint = [stencil2| 1 4 1
>                        4 0 4
>                        1 4 1 |]

We modify our solver above to take a stencil and also an *Int* which
is used to normalise the factors in the stencil. For example, in the
five point method this is 4.

> solveLaplaceStencil :: Monad m
>                        => Int
>                        -> Stencil DIM2 Double
>                        -> Int
>                        -> Array U DIM2 Double
>                        -> Array U DIM2 Double
>                        -> Array U DIM2 Double
>                        -> m (Array U DIM2 Double)
> solveLaplaceStencil !steps !st !nF !arrBoundMask !arrBoundValue !arrInit
>  = go steps arrInit
>  where
>    go 0 !arr = return arr
>    go n !arr
>      = do arr' <- relaxLaplace arr
>           go (n - 1) arr'
>
>    relaxLaplace arr
>      = computeP
>      $ R.szipWith (+) arrBoundValue
>      $ R.szipWith (*) arrBoundMask
>      $ R.smap (/ (fromIntegral nF))
>      $ mapStencil2 (BoundConst 0)
>      st arr

We can then test both methods.

> testStencil5 :: Int -> Int -> IO (Array U DIM2 Double)
> testStencil5 gridSize nIter = do
>   mask    <- boundMask gridSize gridSize
>   val     <- boundValue gridSize gridSize bndFnEg1
>   initArr <- mkInitArrM gridSize
>   solveLaplaceStencil nIter fivePoint 4 mask val initArr

    [ghci]
    testStencil5 5 178 >>= return . pPrint

> testStencil9 :: Int -> Int -> IO (Array U DIM2 Double)
> testStencil9 gridSize nIter = do
>   mask    <- boundMask gridSize gridSize
>   val     <- boundValue gridSize gridSize bndFnEg1
>   initArr <- mkInitArrM gridSize
>   solveLaplaceStencil nIter ninePoint 20 mask val initArr

    [ghci]
    testStencil9 5 178 >>= return . pPrint

We note that the methods give different answers. Before explaining
this, let us examine one more example where the exact solution is
known.

We take the example from [@iserles2009first Chapter 8] where the
boundary conditions are:

$$
\begin{aligned}
\phi(x, 0) &= 0 \\
\phi(x, 1) &= \frac{1}{(1 + x)^2 + 1} \\
\phi(0, y) &= \frac{y}{1 + y^2} \\
\phi(1, y) &= \frac{y}{4 + y^2}
\end{aligned}
$$

This has the exact solution

$$
u(x, y) = \frac{y}{(1 + x)^2 + y^2}
$$

And we can calculate the values of this function on a grid.

> analyticValue :: Monad m => Int -> m (Array U DIM2 Double)
> analyticValue gridSize = computeP $ fromFunction (Z :. gridSize + 1 :. gridSize + 1) f
>   where
>     f (Z :. ix :. iy) = y / ((1 + x)^2 + y^2)
>       where
>         y = fromIntegral iy / fromIntegral gridSize
>         x = fromIntegral ix / fromIntegral gridSize

Let us also solve it using the Jacobi method with a five point stencil
and a nine point stencil. Here is the encoding of the boundary values.

> bndFnEg3 :: Int -> Int -> (Int, Int) -> Double
> bndFnEg3 _ m (0, j) |           j >= 0 && j <  m = y / (1 + y^2)
>   where y = (fromIntegral j) / (fromIntegral m)
> bndFnEg3 n m (i, j) | i == n && j >  0 && j <= m = y / (4 + y^2)
>   where y = fromIntegral j / fromIntegral m
> bndFnEg3 n _ (i, 0) |           i >  0 && i <= n = 0.0
> bndFnEg3 n m (i, j) | j == m && i >= 0 && i <  n = 1 / ((1 + x)^2 + 1)
>   where x = fromIntegral i / fromIntegral n
> bndFnEg3 _ _ _                                   = 0.0

We create a function to run a solver.

> runSolver ::
>   Monad m =>
>   Int ->
>   Int ->
>   (Int -> Int -> (Int, Int) -> Double) ->
>   (Int ->
>    Array U DIM2 Double ->
>    Array U DIM2 Double ->
>    Array U DIM2 Double ->
>    m (Array U DIM2 Double)) ->
>   m (Array U DIM2 Double)
> runSolver nGrid nIter boundaryFn solver = do
>   mask    <- boundMask nGrid nGrid
>   val     <- boundValue nGrid nGrid boundaryFn
>   initArr <- mkInitArrM nGrid
>   solver nIter mask val initArr

> foo = do
>   exact <- analyticValue 7
>   jacobi5 <- runSolver 7 200 bndFnEg3 (solveLaplaceStencil 200 fivePoint)
>   jacobi9 <- runSolver 7 200 bndFnEg3 (solveLaplaceStencil 200 ninePoint)
>   putStrLn $ render $ pPrint $ R.map abs $ (exact -^ jacobi5)
>   putStrLn $ render $ pPrint $ R.map abs $ (exact -^ jacobi9)

> exact :: Int -> Int -> IO (Array U DIM2 Double)
> exact n m = computeP $ fromFunction (Z :. (n + 1) :. (m + 1)) f
>   where
>     f (Z :. i :. j) = y / ((1 + x)^2 + y^2)
>       where
>         x = fromIntegral i / fromIntegral n
>         y = fromIntegral j / fromIntegral m

> solveLaplaceStencil9 :: Monad m
>                        => Int
>                        -> Array U DIM2 Double
>                        -> Array U DIM2 Double
>                        -> Array U DIM2 Double
>                        -> m (Array U DIM2 Double)
> solveLaplaceStencil9 !steps !arrBoundMask !arrBoundValue !arrInit
>  = go steps arrInit
>  where
>    go 0 !arr = return arr
>    go n !arr
>      = do arr' <- relaxLaplace arr
>           go (n - 1) arr'
>
>    relaxLaplace arr
>      = computeP
>      $ R.szipWith (+) arrBoundValue
>      $ R.szipWith (*) arrBoundMask
>      $ R.smap (/ 20)
>      $ mapStencil2 (BoundConst 0)
>      ninePoint arr


Bibliography
============
