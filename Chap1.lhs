% Laplace's Equation: Haskell and C
% Dominic Steinitz
% 15th January 2014

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

Laplace's Equation: The Five Point Formula
==========================================

In this chapter, we show how to apply a numerical method to [Laplace's](Laplace) equation:

  [Laplace]: http://en.wikipedia.org/wiki/Laplace's_equation

$$
\nabla^2 \phi = 0
$$

We take the example from Chapter 8 of "A First Course in the Numerical Analysis of Differential Equations" where the boundary conditions are:

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


> {-# LANGUAGE BangPatterns #-}
> import Data.Array.Repa                  as R
> import Data.Array.Repa.Unsafe           as R
> import qualified Data.Array.Repa.Shape  as S

> import Data.Array.Repa.IO.Timing

> import SolverGet                as SG
> import SolverStencil            as SS
> import Prelude                  as P
> import Text.Printf
> import Options.Applicative



-- | Solver for the Laplace equation.

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

> main = execParser opts >>= main'
>   where
>     opts = info (helper <*> options)
>            fullDesc

> main' :: Options -> IO ()
> main' os = do
>   bv <- boundValue (gridWidth os) (gridHeight os)
>   bm <- boundMask  (gridWidth os) (gridHeight os)
>   sl1 <- SG.solveLaplace 1 bm bv bv
>   sl2 <- analyticValue
>   putStrLn $ show $ sl1!(Z :. midPoint :. midPoint)
>   putStrLn $ show $ sl2!(Z :. midPoint :. midPoint)
>   let vss = toList sl1
>   mapM_ (\v -> putStrLn $ printf "%16.10e" v) vss

> boundValue :: Monad m => Int -> Int -> m (Array U DIM2 Double)
> boundValue gridSizeX gridSizeY = computeP $
>                                  fromFunction (Z :. gridSizeX + 1 :. gridSizeY + 1) f
>   where
>     f (Z :. ix :. iy) | iy == 0         = 0
>     f (Z :. ix :. iy) | iy == gridSizeY = 1 / ((1 + x)^2 + 1)
>       where
>         x = fromIntegral ix / fromIntegral gridSizeX
>     f (Z :. ix :. iy) | ix == 0         = y / (1 + y^2)
>       where
>         y = fromIntegral iy / fromIntegral gridSizeY
>     f (Z :. ix :. iy) | ix == gridSizeX = y / (4 + y^2)
>       where
>         y = fromIntegral iy / fromIntegral gridSizeY
>     f _                                 = 0

> boundMask :: Monad m => Int -> Int -> m (Array U DIM2 Double)
> boundMask gridSizeX gridSizeY = computeP $
>                                 fromFunction (Z :. gridSizeX + 1 :. gridSizeY + 1) f
>   where
>     f (Z :. ix :. iy) | iy == 0         = 0
>     f (Z :. ix :. iy) | iy == gridSizeY = 0
>     f (Z :. ix :. iy) | ix == 0         = 0
>     f (Z :. ix :. iy) | ix == gridSizeX = 0
>     f _                                 = 1

> analyticValue :: Monad m => m (Array U DIM2 Double)
> analyticValue = computeP $ fromFunction (Z :. gridSize + 1 :. gridSize + 1) f
>   where
>     f (Z :. ix :. iy) = y / ((1 + x)^2 + y^2)
>       where
>         y = fromIntegral iy / fromIntegral gridSize
>         x = fromIntegral ix / fromIntegral gridSize

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

Some words after the example.
