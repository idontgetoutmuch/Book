{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

module TestDiag (
  displayGrid
  ) where

import Data.Array.Repa hiding ( map, (++) )
import Data.List.Split ( chunksOf )

import Text.Printf ( printf )

import Diagrams.Prelude hiding ( render )
import Diagrams.Backend.CmdLine
import Diagrams.Backend.Cairo.CmdLine ( B ) -- FIXME: It seems we should
                                            -- be able to use a type
                                            -- variable rather than
                                            -- having to import this

-- FIXME: Disgusting - we assume that 0.0 means something special. We
-- should use Maybe.
-- FIXME: c is not used.
gridSq :: Double -> Double -> (Double, (Int, Int)) -> Diagram B R2
gridSq minX maxX (x, n) = text (printf "%2.2f" x) # scale 0.2 # fc white
                               <> square 1 # lw 0 # fc (getColour x) # named n

  where

    getColour x | x == 0.0 = grey
    getColour x            = blend ((x - minX) / (maxX - minX)) red blue

-- FIXME: remove gridNum - it has no use here - it might be possible
-- to make the labels polymorphic

grid :: Int -> [Double] -> Diagram B R2
grid n vs = if aLen == sLen
            then result
            else error $ "Specified grid size " ++ show sLen ++
                         " Actual grid size "   ++ show aLen
  where
    aLen = length vs
    sLen = n * n
    result = vcat $
             map hcat $
             map (map (gridSq 1.0 2.0)) $
             chunksOf n (zip vs [(i, j) | i <- [1..n], j <- [1..n]])

displayGrid :: Int -> Array U DIM2 Double -> FilePath -> IO ()
displayGrid n ts fn =
  mainRender ( DiagramOpts (Just 900) (Just 600) fn
             , DiagramLoopOpts False Nothing 0)
             (valuedGrid n ts)

valuedGrid :: Int -> Array U DIM2 Double -> Diagram B R2
valuedGrid n ts =
  grid (n+1) (toList ts)
