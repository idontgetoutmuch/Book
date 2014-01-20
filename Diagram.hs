{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagram (
    example
  , main
  ) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.List
import Data.List.Split

gridLineWidth :: Double
gridLineWidth = 0.001

tick :: (Int, Int) -> Diagram B R2
tick  (n, m) = pointDiagram origin # named (n, m)

gridWithHalves :: Int -> Int -> Diagram B R2
gridWithHalves n m = mconcat lineXs <>
           mconcat lineYs <>
           (intersections # translate (r2 (0.0 - delta2X ,1.0 + delta2Y)))

  where

    deltaX   = 1 / fromIntegral n
    deltaY   = 1 / fromIntegral m
    delta2X  = 1 / fromIntegral (2 * n)
    delta2Y  = 1 / fromIntegral (2 * m)

    ns  = [0..n]
    ms  = [0..m]
    n2s = [0..2 * n + 2]
    m2s = [0..2 * m + 2]

    xs = map ((* deltaX)  . fromIntegral) ns
    ys = map ((* deltaY)  . fromIntegral) ms

    lineXs = Prelude.map lineX ys
    lineYs = Prelude.map lineY xs

    lineX y = fromOffsets [r2 (1.0, 0.0) ^-^ r2 (0.0, 0.0)] #
              translate (r2 (0.0, y)) #
              lc red #
              lw gridLineWidth

    lineY x = fromOffsets [r2 (0.0, 1.0) ^-^ r2 (0.0, 0.0)] #
              translate (r2 (x, 0.0)) #
              lc blue #
              lw gridLineWidth

    intersections = hcat $
                    intersperse (strutX delta2X) $
                    map vcat $
                    map (intersperse (strutY delta2Y)) $
                    chunksOf (2 * m + 1 + 2) [ tick (n, m) | n <- n2s, m <- m2s ]

cSize :: Double
cSize = 0.03

intPt, bndPt :: Diagram B R2
intPt = circle (cSize / 2) # fc blue # opacity 0.5 # lw 0
bndPt = circle (cSize / 2) # fc red  # opacity 0.5 # lw 0

txtPt :: String -> Diagram B R2
txtPt t = circle cSize # opacity 0.0 # lw 0.0 === text t # scaleX 0.03 # scaleY 0.03

intPts :: IsName n => [n] -> Diagram B R2 -> Diagram B R2
intPts = flip $ foldr (\n -> withName n (atop . place intPt . location))

bndPts :: IsName n => [n] -> Diagram B R2 -> Diagram B R2
bndPts = flip $ foldr (\n -> withName n (atop . place bndPt . location))

n, m :: Int
n = 3
m = 3

fivePointList :: Int -> Int -> [(Int, Int)]
fivePointList n m = [ (n - 1, m - 1)
                    , (n - 1, m + 1)
                    , (n + 1, m + 1)
                    , (n + 1, m + 3)
                    , (n + 3, m + 3)
                    , (n + 3, m + 1)
                    , (n + 5, m + 1)
                    , (n + 5, m - 1)
                    , (n + 3, m - 1)
                    , (n + 3, m - 3)
                    , (n + 1, m - 3)
                    , (n + 1, m - 1)
                    , (n - 1, m - 1)
                    ]

-- FIXME: Parameterize this so if we change m and n we get a sensible stencil
fivePointPairs :: [((Int, Int), (Int, Int))]
fivePointPairs = zip (fivePointList 3 3) (tail (fivePointList 3 3))

fiveLine :: (IsName a, IsName b) => (a, b) -> Diagram B R2 -> Diagram B R2
fiveLine (a, b) =
  withName a $ \x1 ->
  withName b $ \x2 ->
  atop ((location x1 ~~ location x2) # lc black # lw 0.003 dashing [0.01,0.01] 0)

fiveLines :: Diagram B R2 -> Diagram B R2
fiveLines = foldr (.) id (map fiveLine fivePointPairs)

-- FIXME: Also parameterize this
example :: Diagram B R2
example = (gridWithHalves n m) #
          fiveLines #
          intPts [(n + 1,          m + 1) | n <- [2,4..2 * n - 1] :: [Int]
                                          , m <- [2,4..2 * m - 1] :: [Int]] #
          bndPts [(1 :: Int,  m + 1     ) | m <- [0,2..2 * m]] #
          bndPts [(n + 1,     1 :: Int  ) | n <- [0,2..2 * n]] #
          bndPts [(2 * n + 1, m + 1     ) | m <- [0,2..2 * m]] #
          bndPts [(n + 1,     2 * m + 1 ) | n <- [0,2..2 * n]] #
          withName (3  :: Int,  3 :: Int) (atop . place (txtPt "u_11" # fc blue) . location) #
          withName (5 :: Int,  3 :: Int) (atop . place (txtPt "u_21" # fc blue) . location) #
          withName (7 :: Int,  3 :: Int) (atop . place (txtPt "u_31" # fc red) . location) #
          withName (5 :: Int,  1 :: Int) (atop . place (txtPt "u_20" # fc red) . location) #
          withName (5 :: Int,  5 :: Int) (atop . place (txtPt "u_22" # fc blue) . location)

main :: IO ()
main = mainWith example
