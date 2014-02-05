% Laplace's Equation: Haskell and C
% Dominic Steinitz
% 15th January 2014

---
bibliography: Book.bib
---

\usepackage{color}

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

In Haskell terms this gives us the opportunity to use the repa
library, compare it against a similar implementation in C and use
[hmatrix](http://hackage.haskell.org/package/hmatrix) which is based
on [Lapack](http://www.netlib.org/lapack/) (as well as other
libraries) albeit hmatrix only to illustrate a numerical method which
we use to solve Laplace's equation.

One way in which using repa stands out from the equivalent C
implementation is that it gives a language in which we can specify the
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

 5. The corners are grey because in the finite difference method these
play no part in determining temperatures in the interior of the plate.


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

{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

> {-# LANGUAGE BangPatterns                  #-}
> {-# LANGUAGE TemplateHaskell               #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE NoMonomorphismRestriction     #-}

> module Chap1 (
>     test
>   , slnHMat
>   , test5
>   , slnHMat5
>   , boundMask
>   , boundValue
>   , bndFnEg1
>   , midPoint
>   , main'
>   , main
>   ) where
>
> import Data.Array.Repa                   as R
> import Data.Array.Repa.Unsafe            as R
> import Data.Array.Repa.Algorithms.Matrix
> import Data.Array.Repa.Stencil           as A
> import Data.Array.Repa.Stencil.Dim2      as A

> import qualified SolverStencil           as SS
> import Prelude                           as P

> import Text.Printf
> import Options.Applicative

> import Data.Packed.Matrix
> import Numeric.LinearAlgebra.Algorithms

> import Text.PrettyPrint.HughesPJClass ( render, pPrint )
> import PrettyPrint ()

> import Chap1Aux

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

We disretize (much more detail from Iserles' book needed here).

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

First we create a matrix in *hmatrix* form

> matHMat = do
>   matRepa <- computeP $ mkJacobiMat 3 :: IO (Array U DIM2 Double)
>   return $ 4 >< 4 $ toList matRepa

    [ghci]
    matHMat

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

> bndHMat = 4 >< 1 $ mkJacobiBnd fromIntegral bnd1 3

    [ghci]
     bndHMat

> slnHMat = matHMat >>= return . flip linearSolve bndHMat

    [ghci]
    slnHMat

The Jacobi Method
-----------------

Inverting a matrix is expensive so instead we use the (possibly most)
classical of all iterative methos, Jacobi iteration. Given
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
x_{i,j}^{[k+1]} = \frac{1}{4}(x_{i-1,j} + x_{i,j-1} + x_{i+1,j} + x_{i,j+1})
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

Better would be to use at least a *Bool* as the example below show but
we wish to modify the code from the [repa git
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

> initArrM :: IO (Array U DIM2 Double)
> initArrM = computeP $ fromFunction (Z :. 4 :. 4) (const 0.0)

We can now test the Jacobi method

> test :: Int -> IO (Array U DIM2 Double)
> test nIter = do
>   mask    <- boundMask 3 3
>   val     <- boundValue 3 3 bndFnEg1
>   initArr <- initArrM
>   solveLaplace nIter mask val initArr

After 55 iterations, we obtain convergence up to the limit of accuracy
of double precision floating point numbers.

    [ghci]
    test 55 >>= return . pPrint

A Larger Example
----------------

> matHMat5 = do
>   matRepa <- computeP $ mkJacobiMat 5 :: IO (Array U DIM2 Double)
>   return $ 16 >< 16 $ toList matRepa

    [ghci]
    matHMat5

> bndHMat5 = 16 >< 1 $ mkJacobiBnd fromIntegral bnd1 5

    [ghci]
     bndHMat5

> slnHMat5 = matHMat5 >>= return . flip linearSolve bndHMat5

    [ghci]
    slnHMat5

> initArr5M :: IO (Array U DIM2 Double)
> initArr5M = computeP $ fromFunction (Z :. 6 :. 6) (const 0.0)

> mkInitArrM :: Monad m => Int -> m (Array U DIM2 Double)
> mkInitArrM n = computeP $ fromFunction (Z :. (n + 1) :. (n + 1)) (const 0.0)
>
> test5 :: Int -> IO (Array U DIM2 Double)
> test5 nIter = do
>   mask    <- boundMask 5 5
>   val     <- boundValue 5 5 bndFnEg1
>   initArr <- initArr5M
>   solveLaplace nIter mask val initArr

    [ghci]
    test5 178 >>= return . pPrint

With a larger grid we need more points (178) before the Jacobi method
converges.

Stencils
========

> fivePoint :: Stencil DIM2 Double
> fivePoint = [stencil2|  0 1 0
>                         1 0 1
>                         0 1 0 |]

> ninePoint :: Stencil DIM2 Double
> ninePoint = [stencil2| 1 4 1
>                        4 0 4
>                        1 4 1 |]

> solveLaplaceStencil :: Monad m
>                        => Int
>                        -> Array U DIM2 Double
>                        -> Array U DIM2 Double
>                        -> Array U DIM2 Double
>                        -> m (Array U DIM2 Double)
> solveLaplaceStencil !steps !arrBoundMask !arrBoundValue !arrInit
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
>      $ R.smap (/ 4)
>      $ mapStencil2 (BoundConst 0)
>      fivePoint arr

> solveLaplaceStencil' :: Monad m
>                        => Int
>                        -> Stencil DIM2 Double
>                        -> Int
>                        -> Array U DIM2 Double
>                        -> Array U DIM2 Double
>                        -> Array U DIM2 Double
>                        -> m (Array U DIM2 Double)
> solveLaplaceStencil' !steps !st !nF !arrBoundMask !arrBoundValue !arrInit
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

> testStencil5 :: Int -> IO (Array U DIM2 Double)
> testStencil5 nIter = do
>   mask    <- boundMask 5 5
>   val     <- boundValue 5 5 bndFnEg1
>   initArr <- initArr5M
>   solveLaplaceStencil' nIter fivePoint 4 mask val initArr

    [ghci]
    testStencil5 178 >>= return . pPrint

> testStencil9 :: Int -> IO (Array U DIM2 Double)
> testStencil9 nIter = do
>   mask    <- boundMask 5 5
>   val     <- boundValue 5 5 bndFnEg1
>   initArr <- initArr5M
>   solveLaplaceStencil' nIter ninePoint 20 mask val initArr

    [ghci]
    testStencil9 150 >>= return . pPrint

Computational stencil as in page 149?

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

> bndFnEg2 :: Int -> Int -> (Int, Int) -> Double
> bndFnEg2 _ m (0, j) |           j >= 0 && j <  m = 1.0
> bndFnEg2 n m (i, j) | i == n && j >  0 && j <= m = 2.0
> bndFnEg2 n _ (i, 0) |           i >  0 && i <= n = 1.0
> bndFnEg2 n m (i, j) | j == m && i >= 0 && i <  n = 2.0
> bndFnEg2 _ _ _                                   = 0.0

> bndFnEg3 :: Int -> Int -> (Int, Int) -> Double
> bndFnEg3 _ m (0, j) |           j >= 0 && j <  m = y / (1 + y^2)
>   where y = (fromIntegral j) / (fromIntegral m)
> bndFnEg3 n m (i, j) | i == n && j >  0 && j <= m = y / (4 + y^2)
>   where y = fromIntegral j / fromIntegral m
> bndFnEg3 n _ (i, 0) |           i >  0 && i <= n = 0.0
> bndFnEg3 n m (i, j) | j == m && i >= 0 && i <  n = 1 / ((1 + x)^2 + 1)
>   where x = fromIntegral i / fromIntegral n
> bndFnEg3 _ _ _                                   = 0.0


> exact :: Int -> Int -> IO (Array U DIM2 Double)
> exact n m = computeP $ fromFunction (Z :. (n + 1) :. (m + 1)) f
>   where
>     f (Z :. i :. j) = y / ((1 + x)^2 + y^2)
>       where
>         x = fromIntegral i / fromIntegral n
>         y = fromIntegral j / fromIntegral m

> reallyTest ::
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
> reallyTest nGrid nIter boundaryFn solver = do
>   mask    <- boundMask nGrid nGrid
>   val     <- boundValue nGrid nGrid boundaryFn
>   initArr <- mkInitArrM nGrid
>   solver nIter mask val initArr

> boundValueAlt :: Monad m => Int -> Int -> m (Array U DIM2 Double)
> boundValueAlt gridSizeX gridSizeY = computeP $
>                                  fromFunction (Z :. gridSizeX + 1 :. gridSizeY + 1) f
>   where
>     f (Z :. _ix :. iy) | iy == 0         = 0
>     f (Z :.  ix :. iy) | iy == gridSizeY = 1 / ((1 + x)^2 + 1)
>       where
>         x = fromIntegral ix / fromIntegral gridSizeX
>     f (Z :.  ix :. iy) | ix == 0         = y / (1 + y^2)
>       where
>         y = fromIntegral iy / fromIntegral gridSizeY
>     f (Z :.  ix :. iy) | ix == gridSizeX = y / (4 + y^2)
>       where
>         y = fromIntegral iy / fromIntegral gridSizeY
>     f _                                 = 0

-- | Perform matrix relaxation for the Laplace equation,
--	using a stencil function.
--
--   Computation fn is
--	u'(i,j) = (u(i-1,j) + u(i+1,j) + u(i,j-1) + u(i,j+1)) / 4
--
--  Apply the boundary conditions to this matrix.
--	The mask  matrix has 0 in places where boundary conditions hold
--	and 1 otherwise.
--
--	The value matrix has the boundary condition value in places where it holds,
--	and 0 otherwise.
--


> data Options =
>   Options
>   { gridWidth   :: Int
>   , gridHeight  :: Int
>   , _iterations :: Int
>   } deriving Show

> options :: Parser Options
> options = Options
>   <$> option
>       (    long "width"
>         <> short 'w'
>         <> metavar "INT"
>         <> help "Width of grid"
>       )
>   <*> option
>       (    long "depth"
>         <> short 'd'
>         <> metavar "INT"
>         <> help "Height (or depth) of grid"
>       )
>   <*> option
>       (    long "iterations"
>         <> short 'i'
>         <> metavar "INT"
>         <> help "Number of iterations to perform"
>       )

> gridSize :: Int
> gridSize = 2
>
> midPoint :: Int
> midPoint = gridSize `div` 2

> analyticValue :: Monad m => m (Array U DIM2 Double)
> analyticValue = computeP $ fromFunction (Z :. gridSize + 1 :. gridSize + 1) f
>   where
>     f (Z :. ix :. iy) = y / ((1 + x)^2 + y^2)
>       where
>         y = fromIntegral iy / fromIntegral gridSize
>         x = fromIntegral ix / fromIntegral gridSize

> main' :: Options -> IO ()
> main' os = do
>   bv <- boundValueAlt (gridWidth os) (gridHeight os)
>   bm <- boundMask  (gridWidth os) (gridHeight os)
>   sl1 <- solveLaplace 1 bm bv bv
>   sl2 <- analyticValue
>   putStrLn $ show $ sl1!(Z :. midPoint :. midPoint)
>   putStrLn $ show $ sl2!(Z :. midPoint :. midPoint)
>   let vss = toList sl1
>   mapM_ (\v -> putStrLn $ printf "%16.10e" v) vss

> main :: IO ()
> main = execParser opts >>= main'
>   where
>     opts = info (helper <*> options)
>            fullDesc

Comparison C Code
=================

Main
----

This is an example of C.

 ~~~~ {.c include="Chap1a.c"}
 ~~~~

Matrix
------

 ~~~~ {.c include="libc/Matrix.c"}
 ~~~~

Bibliography
============
