{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

module RedBlackStencilOpt
     (solveLaplace) 
where
    
import Data.Array.Repa				as A
import Data.Array.Repa.Stencil			as A
import Data.Array.Repa.Stencil.Dim2		as A


-- | Solver for the Laplace equation.
solveLaplace::
  Monad m
  => Int			-- ^ Number of iterations to use.
  -> Double		        -- ^ weight for over relaxing (>0.0 and <2.0).
  -> Array U DIM2 Double	-- ^ Boundary value mask.
  -> Array U DIM2 Double	-- ^ Boundary values.
  -> Array U DIM2 Double	-- ^ Initial state. Should have even number of columns
  -> m (Array U DIM2 Double)

solveLaplace !steps !omega !arrBoundMask !arrBoundValue !arrInit
 =  do redBoundMask    <- computeP $ projectRed arrBoundMask
       blackBoundMask  <- computeP $ projectBlack arrBoundMask
       redBoundValue   <- computeP $ projectRed arrBoundValue
       blackBoundValue <- computeP $ projectBlack arrBoundValue
       redInit         <- computeP $ projectRed arrInit
       blackInit       <- computeP $ projectBlack arrInit
       iterateLaplace steps omega redInit blackInit
                      redBoundValue blackBoundValue redBoundMask blackBoundMask


iterateLaplace ::
  Monad m
  => Int
  -> Double
  -> Array U DIM2 Double
  -> Array U DIM2 Double
  -> Array U DIM2 Double
  -> Array U DIM2 Double
  -> Array U DIM2 Double
  -> Array U DIM2 Double
  -> m (Array U DIM2 Double)

iterateLaplace !steps !omega !redInit !blackInit
               !redBoundValue !blackBoundValue !redBoundMask !blackBoundMask
     = go steps redInit blackInit
       where
         go 0 !r !b = computeP $ combineRB r b -- return final combined array
         go n !r !b
            = do r' <- computeP
                       $ relaxStep r b redBoundValue redBoundMask leftSt rightSt
                 b' <- computeP
                       $ relaxStep b r' blackBoundValue blackBoundMask rightSt leftSt
                 go (n - 1) r' b'

         {-# INLINE relaxStep #-}
         relaxStep !arrOld !arrNbs !boundValue !boundMask !stencil1 !stencil2
            = altRows (f stencil1) (f stencil2)
              where
                    {-# INLINE f #-}
                    f s = A.szipWith (+) boundValue
                          $ A.szipWith (*) boundMask
                          $ A.szipWith weightedSum arrOld
                          $ A.smap (/4)
                          $ mapStencil2 (BoundConst 0) s arrNbs

         {-# INLINE weightedSum #-}
         weightedSum !old !new = (1-omega)*old + omega*new
  
{-# INLINE iterateLaplace #-}

combineRB :: Array U DIM2 Double -> Array U DIM2 Double -> Array D DIM2 Double
combineRB r b =     -- arr(i,j)
                    --     | even(i+j) = r(i, j `div` 2)
                    --     | otherwise = b(i, j `div` 2)
                    -- arr has i <- 0..n-1 , j <- 0..2m-1
                    -- r   has i <- 0..n-1 , j <- 0..m-1
                    -- b   has i <- 0..n-1 , j <- 0..m-1
    if extent r == extent b
    then  traverse2 r b
                    (\ (e :. i :. j) _ -> (e :. i :. 2*j))
                    (\ get1 get2 (e :. i :. j) -> 
                         (if even(i+j) then get1 else get2)
                                (e :. i :. j `div` 2)
                    )
    else undefined

{-# INLINE combineRB #-}

projectRed :: Array U DIM2 Double -> Array D DIM2 Double
projectRed arr =
    -- Expects even number of columns for arr
                     -- r(i,j) = arr(i, 2*j + (i `mod` 2))
                     -- arr has i <- 0..n-1, j <- 0..2m-1
                     -- r   has i <- 0..n-1 , j <- 0..m-1
     let (_ :. j) = extent arr              
     in  if even j 
         then traverse arr 
                       (\ (e :. i :. j) -> (e :. i :. (j `div` 2)))
                       (\get (e :. i :. j) -> get (e :. i :. 2*j + (i `mod` 2)))
         else undefined
        
{-# INLINE projectRed #-}

projectBlack :: Array U DIM2 Double -> Array D DIM2 Double
projectBlack arr =
    -- Expects even number of columns for arr
                     -- b(i,j) = arr(i, 2*j + ((i+1) `mod` 2))
                     -- arr has i <- 0..n-1, j <- 0..2m-1
                     -- b   has i <- 0..n-1 , j <- 0..m-1
     let (_ :. j) = extent arr              
     in  if even j 
         then traverse arr 
                       (\ (e :. i :. j) -> (e :. i :. (j `div` 2)))
                       (\get (e :. i :. j) -> get (e :. i :. 2*j + ((i+1) `mod` 2)))
         else undefined

{-# INLINE projectBlack #-}

altRows :: forall r1 r2 a . (Source r1 a, Source r2 a)
        => Array r1 DIM2 a -> Array r2 DIM2 a -> Array D DIM2 a  
                
altRows !arr1 !arr2 = 
    -- alternates rows from 2 arrays (assumed to be the same extent)
    if extent arr1 == extent arr2
    then traverse2 arr1 arr2
                   (\ e _ -> e)
                   (\ get1 get2 e@(_ :. i :. _) -> 
                      if even i then get1 e else get2 e
                   )
    else undefined
                     
{-# INLINE altRows #-}

-- Stencils

leftSt :: Stencil DIM2 Double -- even rows from b, odd from r
leftSt  =   [stencil2|  0 1 0         
                        1 1 0 
                        0 1 0 |]

rightSt :: Stencil DIM2 Double -- odd rows from b, even from r
rightSt =   [stencil2|  0 1 0 
                        0 1 1 
                        0 1 0 |]


{-# NOINLINE solveLaplace #-}
