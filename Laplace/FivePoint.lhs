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

> import Data.Array.Repa.IO.Timing

> import SolverGet                as SG
> import SolverStencil            as SS
> import Data.Array.Repa          as R
> import Prelude                  as P
> import Text.Printf

> gridSize :: Int
> gridSize = 2
> 
> midPoint :: Int
> midPoint = gridSize `div` 2

> main :: IO ()
> main = do
>   bv <- boundValue
>   bm <- boundMask
>   sl1 <- SG.solveLaplace 1 bm bv bv
>   sl2 <- analyticValue
>   putStrLn $ show $ sl1!(Z :. midPoint :. midPoint)
>   putStrLn $ show $ sl2!(Z :. midPoint :. midPoint)
>   let vss = toList sl1
>   mapM_ (\v -> putStrLn $ printf "%16.10e" v) vss

> boundValue :: Monad m => m (Array U DIM2 Double)
> boundValue = computeP $ fromFunction (Z :. gridSize + 1 :. gridSize + 1) f
>   where
>     f (Z :. ix :. iy) | iy == 0        = 0
>     f (Z :. ix :. iy) | iy == gridSize = 1 / ((1 + x)^2 + 1)
>       where
>         x = fromIntegral ix / fromIntegral gridSize
>     f (Z :. ix :. iy) | ix == 0        = y / (1 + y^2)
>       where
>         y = fromIntegral iy / fromIntegral gridSize
>     f (Z :. ix :. iy) | ix == gridSize = y / (4 + y^2)
>       where
>         y = fromIntegral iy / fromIntegral gridSize
>     f _                                = 0

> boundMask :: Monad m => m (Array U DIM2 Double)
> boundMask = computeP $ fromFunction (Z :. gridSize + 1 :. gridSize + 1) f
>   where
>     f (Z :. ix :. iy) | iy == 0        = 0
>     f (Z :. ix :. iy) | iy == gridSize = 0
>     f (Z :. ix :. iy) | ix == 0        = 0
>     f (Z :. ix :. iy) | ix == gridSize = 0
>     f _                                = 1

> analyticValue :: Monad m => m (Array U DIM2 Double)
> analyticValue = computeP $ fromFunction (Z :. gridSize + 1 :. gridSize + 1) f
>   where
>     f (Z :. ix :. iy) = y / ((1 + x)^2 + y^2)
>       where
>         y = fromIntegral iy / fromIntegral gridSize
>         x = fromIntegral ix / fromIntegral gridSize
