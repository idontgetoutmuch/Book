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
  , fivePointStencil
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
gridWithHalves n m =
  mconcat lineXs <>
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

arrowStyle1 :: Color a => a -> ArrowOpts
arrowStyle1 c = (with  & arrowHead  .~ noHead
                       & shaftStyle %~ lw 0.005
                       & shaftColor .~ c
                       & arrowTail  .~ noTail)

fivePointStencil :: Diagram B R2
fivePointStencil = points #
                   connectStencil "Centre" "West"  (1/2 :: Turn) (0 :: Turn)   #
                   connectStencil "East" "Centre"  (1/2 :: Turn) (0 :: Turn)   #
                   connectStencil "Centre" "North" (1/4 :: Turn) (3/4 :: Turn) #
                   connectStencil "South" "Centre" (1/4 :: Turn) (3/4 :: Turn)

  where points = (bndPt # named "West" # translate (r2 (0.0, 0.5))) <>
                 (intPt # named "Centre" # translate (r2 (0.5, 0.5))) <>
                 (bndPt # named "East" # translate (r2 (1.0, 0.5))) <>
                 (bndPt # named "North" # translate (r2 (0.5, 1.0))) <>
                 (bndPt # named "South" # translate (r2 (0.5, 0.0)))

        connectStencil n1 n2 o1 o2 =
          connectPerim' (arrowStyle1 green) n1 n2 o1 o2

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

fivePointPairs :: Int -> Int -> [((Int, Int), (Int, Int))]
fivePointPairs n m = let pts = fivePointList n m
                     in zip pts (tail pts)

fiveLine :: (IsName a, IsName b) => (a, b) -> Diagram B R2 -> Diagram B R2
fiveLine (a, b) =
  withName a $ \x1 ->
  withName b $ \x2 ->
  atop ((location x1 ~~ location x2) # lc black # lw 0.003 dashing [0.01,0.01] 0)

fiveLines :: Int -> Int -> Diagram B R2 -> Diagram B R2
fiveLines n m = foldr (.) id (map fiveLine $ fivePointPairs n m)

annotate :: String -> Colour Double -> Int -> Int -> Diagram B R2
annotate s h n m = txtPt (s ++ "_" ++ show n ++ show m) # fc h

annotate' :: String -> Colour Double -> Int -> Int -> Diagram B R2 -> Diagram B R2
annotate' s h n m =
  withName (n, m) (atop . place (annotate s h (n `div` 2) (m `div` 2)) . location)

example :: Int -> Int -> Diagram B R2
example n m =
  (gridWithHalves n m) #
  fiveLines (2 * n - 3) 3 #
  intPts [(n + 1,          m + 1) | n <- [2,4..2 * n - 1] :: [Int]
                                  , m <- [2,4..2 * m - 1] :: [Int]] #
  bndPts [(1 :: Int,  m + 1     ) | m <- [0,2..2 * m]] #
  bndPts [(n + 1,     1 :: Int  ) | n <- [0,2..2 * n]] #
  bndPts [(2 * n + 1, m + 1     ) | m <- [0,2..2 * m]] #
  bndPts [(n + 1,     2 * m + 1 ) | n <- [0,2..2 * n]] #
  annotate' "u" red  (2 * n - 1) 1 #
  annotate' "u" red  (2 * n + 1) 3 #
  annotate' "u" blue (2 * n - 1) 3 #
  annotate' "u" blue (2 * n - 3) 3 #
  annotate' "u" blue (2 * n - 1) 5

main :: IO ()
main = mainWith $ fivePointStencil
