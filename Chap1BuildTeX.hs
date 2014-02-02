module Main (
  main
  ) where

import Chap1Aux
import Chap1 ( boundMask
             , boundValue
             , bndFnEg1
             )

import Data.Array.Repa
import RedBlackStencilOpt
import TestDiag

n :: Int
n = 21

main :: IO ()
main = do
  writeFile "matrix5.tex" (matrix' 5)
  writeFile "matrix3.tex" (matrix' 3)

  bndMsk <- boundMask n n
  bndVal <- boundValue n n bndFnEg1
  intArr <- computeP $ fromFunction (Z :. (n + 1) :. (n + 1)) (const 0.0)

  finArr <- solveLaplace 200 1.0 bndMsk bndVal intArr

  displayGrid n 1 finArr "diagrams/heatmap.png"


