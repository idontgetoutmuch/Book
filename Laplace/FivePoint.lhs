In this chapter, we show how to apply a numerical method to [Laplace's][Laplace] equation:

  [Laplace]: http://en.wikipedia.org/wiki/Laplace's_equation

$$
\nabla^2 \phi = 0
$$

We take the example from Chapter 8 of "A First Course in the Numerical Analysis of Differential Equations" where the boundary conditions are:

\begin{align*}
\phi(x, 0) &= 0 \\
\phi(x, 1) &= \frac{1}{(1 + x)^2 + 1} \\
\phi(0, y) &= \frac{y}{1 + y^2} \\
\phi(1, y) &= \frac{y}{4 + y^2}
\end{align*}

This has the exact solution

$$
u(x, y) = \frac{y}{(1 + x)^2 + y^2}
$$

> {-# LANGUAGE BangPatterns, RankNTypes, FlexibleContexts #-}

> import Data.Array.Repa.IO.Timing

> import SolverGet                as SG
> import SolverStencil            as SS
> import Data.Array.Repa          as R
> import Prelude                  as P

> gridSize :: Int
> gridSize = 5

> main :: IO ()
> main = do
>   bv <- boundValue
>   putStrLn $ show bv
>   bm <- boundMask
>   putStrLn $ show bm
>   sl1 <- SG.solveLaplace 1 bm bv bv
>   putStrLn $ show sl1
>   sl2 <- SG.solveLaplace 2 bm bv bv
>   putStrLn $ show sl2
>   sld21 <- computeP $ sl2 -^ sl1 :: IO (Array U DIM2 Double)
>   putStrLn $ show sld21
>   sl3 <- SG.solveLaplace 50 bm bv bv
>   putStrLn $ show sl3
>   sl4 <- SG.solveLaplace 51 bm bv bv
>   putStrLn $ show sl4
>   sld43 <- computeP $ sl4 -^ sl3 :: IO (Array U DIM2 Double)
>   putStrLn $ show sld43



> boundValue :: Monad m => m (Array U DIM2 Double)
> boundValue = computeP $ fromFunction (Z :. gridSize :. gridSize) f
>   where
>     f (Z :. ix :. iy) | iy == 0            = 0
>     f (Z :. ix :. iy) | iy == gridSize - 1 = 1 / ((1 + x)^2 + 1)
>       where
>         x = fromIntegral ix / fromIntegral gridSize
>     f (Z :. ix :. iy) | ix == 0            = y / (1 + y^2)
>       where
>         y = fromIntegral iy / fromIntegral gridSize
>     f (Z :. ix :. iy) | ix == gridSize - 1 = y / (4 + y^2)
>       where
>         y = fromIntegral iy / fromIntegral gridSize
>     f _                                    = 0

> boundMask :: Monad m => m (Array U DIM2 Double)
> boundMask = computeP $ fromFunction (Z :. gridSize :. gridSize) f
>   where
>     f (Z :. ix :. iy) | iy == 0            = 0
>     f (Z :. ix :. iy) | iy == gridSize - 1 = 0
>     f (Z :. ix :. iy) | ix == 0            = 0
>     f (Z :. ix :. iy) | ix == gridSize - 1 = 0
>     f _                                    = 1
