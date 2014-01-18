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

module Main (main) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Data.List
import Data.List.Split

gridLineWidth :: Double
gridLineWidth = 0.001

tick :: (Int, Int) -> Diagram B R2
tick  (n, m) = pointDiagram origin # named (n, m)

grid :: Int -> Int -> Diagram B R2
grid n m = mconcat lineXs <>
           mconcat lineYs <>
           (intersections # translate (r2 (0.0,1.0)) # showOrigin)

  where

    deltaX  = 1 / fromIntegral n
    deltaY  = 1 / fromIntegral m

    ns = [0..n]
    ms = [0..m]
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
                    intersperse (strutX deltaX) $
                    map vcat $
                    map (intersperse (strutY deltaY)) $
                    chunksOf (m + 1) [ tick (n, m) | n <- ns, m <- ms ]

cSize :: Double
cSize = 0.03

intPt, bndPt :: Diagram B R2
intPt = circle (cSize /2 ) # fc blue # opacity 0.5 # lw 0
bndPt = circle (cSize /2 ) # fc red  # opacity 0.5 # lw 0

intPts :: IsName n => [n] -> Diagram B R2 -> Diagram B R2
intPts = flip $ foldr (\n -> withName n (atop . place intPt . location))

bndPts :: IsName n => [n] -> Diagram B R2 -> Diagram B R2
bndPts = flip $ foldr (\n -> withName n (atop . place bndPt . location))

n, m :: Int
n = 10
m = 5

main :: IO ()
main = mainWith $ (grid n m) #
       connect (0 :: Int, 0 :: Int) (n :: Int, 4 :: Int) #
       connect (0 :: Int, 0 :: Int) ( 0 :: Int, m :: Int) #
       intPts [(n, m)        | n <- [1,2..n - 1] :: [Int], m <- [1,2..m - 1] :: [Int]] #
       bndPts [(0 :: Int, m       ) |                  m <- [0..m]] #
       bndPts [(n,        0 :: Int) |                  n <- [0..n]] #
       bndPts [(n,        m       ) |                  m <- [0..m]] #
       bndPts [(n,        m       ) |                  n <- [0..n]]
