% Laplace's Equation: Haskell and C
% Dominic Steinitz
% 15th January 2014

---
bibliography: Book.bib
---

\usepackage{color}

Introduction
============

We are going to solve Laplace's equation $\nabla^2 \phi = 0 $ in 2
dimensions. One motivation for doing so is that it is a moderately
simple equation (in so far as partial differential equations are
simple) that has been well studied for centuries. We shall give an
example of its use in the steady state heat equation. Furthermore, the
Laplacian itself is also used computer vision for edge detection
(reference required).

In Haskell terms this gives us the opportunity to use the repa
library, compare it against a similar implementation in C and use
H(?)Lapack and similar against base LAPACK itself.

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

The Steady State Heat Equation
==============================

Haskell Preamble
================

> {-# OPTIONS_GHC -Wall                      #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults    #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
> {-# OPTIONS_GHC -fno-warn-missing-methods  #-}
> {-# OPTIONS_GHC -fno-warn-orphans          #-}

> {-# LANGUAGE BangPatterns #-}

> module Chap1 (
>     exSolLapack
>   , midPoint
>   , main'
>   , main
>   ) where
>
> import Data.Array.Repa                  as R
> import Data.Array.Repa.Unsafe           as R

> import qualified SolverStencil          as SS
> import Prelude                          as P
> import Text.Printf
> import Options.Applicative

> import Data.Packed.Matrix
> import Numeric.LinearAlgebra.Algorithms

> import Text.PrettyPrint.HughesPJClass ( render, pPrint )
> import PrettyPrint ()

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
indicates that the point is a boundary point.

```{.dia height='500'}
import Diagram
dia = example 3 3
```

$$
\begin{bmatrix}
-4 &  1 &  1 &  0 \\
 1 & -4 &  0 &  1 \\
 1 &  0 & -4 &  1 \\
 0 &  1 &  1 & -4
\end{bmatrix}
\begin{bmatrix}
{\color{blue}{u_{1,1}}} \\
{\color{blue}{u_{2,1}}} \\
{\color{blue}{u_{1,2}}} \\
{\color{blue}{u_{2,2}}}
\end{bmatrix}
=
\begin{bmatrix}
-{\color{red}{u_{1,0}}} - {\color{red}{u_{0,1}}} \\
-{\color{red}{u_{2,0}}} - {\color{red}{u_{3,1}}} \\
-{\color{red}{u_{0,2}}} - {\color{red}{u_{1,3}}} \\
-{\color{red}{u_{2,3}}} - {\color{red}{u_{3,2}}}
\end{bmatrix}
$$

Jacobi iteration given $A\boldsymbol{x} = \boldsymbol{b}$

$$
\boldsymbol{x}_i^{[k+1]} = \frac{1}{A_{i,i}}\Bigg[\boldsymbol{b}_i - \sum_{j \neq i} A_{i,j}\boldsymbol{x}_j^{[k]}\Bigg]
$$

> m :: Matrix Double
> m = 4 >< 4 $ concat [ [-4,  1,  0,  1]
>                     , [ 1, -4,  1,  0]
>                     , [ 0,  1, -4,  1]
>                     , [ 1,  0,  1, -4]
>                     ]
>
> b :: Matrix Double
> b = 4 >< 1 $ concat [ [-2]
>                     , [-3]
>                     , [-4]
>                     , [-3]
>                     ]
>
> exSolLapack :: [[Double]]
> exSolLapack = toLists $ linearSolve m b

    [ghci]
    exSolLapack

> mkJacobiMat :: (Int, Int) -> Int -> Array D DIM2 Double
> mkJacobiMat (i, j) n = foo
>   where
>     innerN = (n - 1)^2
>     foo
>       | (i, j) == (1, 1)
>       = fromFunction (Z :. innerN :. innerN) f11
>       | (i, j) == (n - 1, 1)
>       = fromFunction (Z :. innerN :. innerN) fn1
>       | (i, j) == (1, n - 1)
>       = fromFunction (Z :. innerN :. innerN) f1n
>       | (i, j) == (n - 1, n - 1)
>       = fromFunction (Z :. innerN :. innerN) fnn
>       where
>         f11 (Z :. k :. l) | k == 0     && l == 0 = -4.0
>         f11 (Z :. k :. l) | k == 1     && l == 0 =  1.0
>         f11 (Z :. k :. l) | k == n - 1 && l == 0 =  1.0
>         f11 _                                    =  0.0
>
>         fn1 (Z :. k :. l) | k == 1 * (n - 1) - 2 && l == 1 =  1.0
>         fn1 (Z :. k :. l) | k == 1 * (n - 1) - 1 && l == 1 = -4.0
>         fn1 (Z :. k :. l) | k == 2 * (n - 1) - 1 && l == 1 =  1.0
>         fn1 _                                              =  0.0
>
>         f1n (Z :. k :. l) | k == innerN - 2 * (n - 1)     && l == innerN - 2 =  1.0
>         f1n (Z :. k :. l) | k == innerN - 1 * (n - 1) + 0 && l == innerN - 2 = -4.0
>         f1n (Z :. k :. l) | k == innerN - 1 * (n - 1) + 1 && l == innerN - 2 =  1.0
>         f1n _                                                                =  0.0
>
>         fnn (Z :. k :. l) | k == innerN - 1 * (n - 1) - 1 && l == innerN - 1 =  1.0
>         fnn (Z :. k :. l) | k == innerN - 1               && l == innerN - 1 = -4.0
>         fnn (Z :. k :. l) | k == innerN - 2               && l == innerN - 1 =  1.0
>         fnn _                                                                =  0.0

> boundValue :: Monad m => Int -> Int -> m (Array U DIM2 Double)
> boundValue gridSizeX gridSizeY = computeP $
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

> boundMask :: Monad m => Int -> Int -> m (Array U DIM2 Double)
> boundMask gridSizeX gridSizeY = computeP $
>                                 fromFunction (Z :. gridSizeX + 1 :. gridSizeY + 1) f
>   where
>     f (Z :. _ix :.  iy) | iy == 0         = 0
>     f (Z :. _ix :.  iy) | iy == gridSizeY = 0
>     f (Z :.  ix :. _iy) | ix == 0         = 0
>     f (Z :.  ix :. _iy) | ix == gridSizeX = 0
>     f _                                 = 1

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

> relaxLaplace
>   :: Monad m
>      => Array U DIM2 Double	-- ^ Boundary condition mask
>      -> Array U DIM2 Double	-- ^ Boundary condition values
>      -> Array U DIM2 Double	-- ^ Initial matrix
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

	-- Check if this element is on the border of the matrix.
	-- If so we can't apply the stencil because we don't have all the neighbours.

>     isBorder !i !j
>       =  (i == 0) || (i >= width  - 1)
>          || (j == 0) || (j >= height - 1)

> solveLaplace
> 	:: Monad m
>         => Int			-- ^ Number of iterations to use.
>         -> Array U DIM2 Double	-- ^ Boundary value mask.
>         -> Array U DIM2 Double	-- ^ Boundary values.
>         -> Array U DIM2 Double	-- ^ Initial state.
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

Computational stencil as in page 149?

```{.dia height='500'}
import Diagram
dia = example 5 5
```

> data Options =
>   Options
>   { gridWidth  :: Int
>   , gridHeight :: Int
>   , iterations :: Int
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
>   bv <- boundValue (gridWidth os) (gridHeight os)
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
