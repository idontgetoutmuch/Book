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

grid :: [Double] -> [Double] -> Diagram B R2
grid xs ys = mconcat lineXs <> mconcat lineYs
  where
    maxX   = maximum xs
    maxY   = maximum ys

    lineYs = Prelude.map lineY xs
    lineXs = Prelude.map lineX ys

    lineY x = fromOffsets [r2 (x, 0), r2 (0, maxX)] # lc blue
    lineX y = fromOffsets [r2 (0, y), r2 (maxY, 0)] # lc red

    tick (x, y) v = endpt # translate (tickShift x y)
    tickShift x y = r2 (x, y)
    endpt         = circle (cSize /2 ) # fc blue # opacity 0.5 # lw 0

main :: IO ()
main = mainWith $ grid (map ((* 0.05) . fromIntegral) [0..20])
                       (map ((* 0.1)  . fromIntegral) [0..10])
