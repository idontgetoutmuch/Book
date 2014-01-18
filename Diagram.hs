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


gridLineWidth :: Double
gridLineWidth = 0.001

cSize :: Double
cSize = 0.01

grid :: Int -> Int -> Diagram B R2
grid n m = mconcat lineXs <>
            mconcat lineYs <>
            mconcat intersections

  where

    deltaX = 1 / fromIntegral n
    deltaY = 1 / fromIntegral m

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

    intersections = [ tick (x, y) | x <- ns, y <- ms ]

    tick (n, m) = endpt #
                  translate (r2 (deltaX * fromIntegral n, deltaY * fromIntegral m)) #
                  named (n, m)

    endpt       = circle (cSize /2 ) # fc blue # opacity 0.5 # lw 0


main :: IO ()
main = mainWith $ grid 10 5
