module Main (
  main
  ) where

import Chap1Aux
import Chap1 ( boundMask
             , boundValue
             , bndFnEg1
             , bndFnEg3
             , analyticValue
             , runSolver
             , s5
             )

import Data.Array.Repa as R
import RedBlackStencilOpt

import TestDiag hiding ( displayGrid )
import Diagram hiding ( main )
import Diagrams.Prelude
import Diagrams.Backend.CmdLine

n :: Int
n = 21

displayGrid :: Int -> Array U DIM2 Double -> FilePath -> IO ()
displayGrid n ts fn =
  mainRender ( DiagramOpts (Just 900) (Just 600) fn
             , DiagramLoopOpts False Nothing 0)
             (valuedGrid n ts)

displayHeader n ts fn =
  mainRender ( DiagramOpts (Just 900) (Just 600) fn
             , DiagramLoopOpts False Nothing 0)
   (((valuedGrid n ts) # scaleX 0.3 # scaleY 0.3 # translate (r2 ( 1.0,  0.0))) <>
    fivePointStencil # scaleX 2.0 # scaleY 2.0   # translate (r2 (-2.0, -4.0))  <>
    ninePointStencil # scaleX 2.0 # scaleY 2.0   # translate (r2 ( 8.5, -4.0)))

main :: IO ()
main = do
  writeFile "matrix5.tex" (matrixTex 5)
  writeFile "matrix3.tex" (matrixTex 3)

  bndMsk <- boundMask n n
  bndVal <- boundValue n n bndFnEg1
  intArr <- computeP $ fromFunction (Z :. (n + 1) :. (n + 1)) (const 0.0)

  finArr <- solveLaplace 200 1.0 bndMsk bndVal intArr

  displayHeader n finArr "diagrams/header.png"

  displayGrid n finArr "diagrams/heatmap.png"

  exactArr <- analyticValue n
  arr5     <- runSolver n 400 bndFnEg3 s5
  errArr5  <- computeP $ R.map abs $ exactArr -^ arr5
  displayGrid n errArr5 "diagrams/err5Heatmap.png"



