{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

module Chap1Aux where

import Prelude         as P
import Data.Array.Repa as R
import Data.List ( foldl' )
import Text.PrettyPrint.HughesPJClass
import PrettyPrint ()

fn1a :: Int -> Doc
fn1a = int

fn1b :: Int -> Double
fn1b = fromIntegral

fn2a :: Int -> [(Int, Int)] -> Doc
fn2a _ = hcat .
         punctuate (text " + ") .
         P.map (\(i, j) -> text "-" <>
                           braces ( text "\\color" <>
                                    braces (text "red") <>
                                    braces (text "u_" <> braces (int i <> int j))
                                  )
               )

fn2b :: Int -> [(Int, Int)] -> Double
fn2b n = negate .
         sum .
         P.map (bndFn n)

ux0, ux1, u0y, u1y :: Int -> Double
ux0 = const 1.0
ux1 = const 2.0
u0y = const 1.0
u1y = const 2.0

bndFn :: Int -> (Int, Int) -> Double
bndFn n (0, j) |           j > 0 && j < n = u0y j
bndFn n (i, j) | i == n && j > 0 && j < n = u1y j
bndFn n (i, 0) |           i > 0 && i < n = ux0 i
bndFn n (i, j) | j == n && i > 0 && i < n = ux1 i
bndFn _ _      = error "Boundary function used at non-boundary point"

mkJacobiBnd :: (Int -> a) -> (Int -> [(Int, Int)] -> a) -> Int -> [a]
mkJacobiBnd fn1 fn2 n = concat [corners, concat edges , inners]

  where

    corners = [pp nw, pp sw, pp ne, pp se]

    edges = P.map edge [west, east, south, north]

    edge fn = P.map (pp . fn) [2..n - 2]

    inners = replicate ((n - 3)^2) $ fn1 0

    pp x = fn2 n $
           P.map snd $
           filter fst $
           toList $
           flatten ((n + 1)^2) x

    north l =  fromFunction (Z :. n + 1 :. n + 1) (nAux l)

    nAux l (Z :. i :. j) | i == l && j == n = (True,  (i, 0))
    nAux _ (Z :. i :. j)                    = (False, (i ,j))

    south l =  fromFunction (Z :. n + 1 :. n + 1) (sAux l)

    sAux l (Z :. i :. j) | i == l && j == n = (True,  (i, n))
    sAux _ (Z :. i :. j)                    = (False, (i ,j))

    east l =  fromFunction (Z :. n + 1 :. n + 1) (eAux l)

    eAux l (Z :. i :. j) | i == n && j == l = (True,  (i, j))
    eAux _ (Z :. i :. j)                    = (False, (i ,j))

    west l =  fromFunction (Z :. n + 1 :. n + 1) (wAux l)

    wAux l (Z :. i :. j) | i == 0 && j == l = (True,  (i, j))
    wAux _ (Z :. i :. j)                    = (False, (i ,j))

    nw = fromFunction (Z :. n + 1 :. n + 1) nwAux

    nwAux (Z :. 0 :. 1) = (True,  (1, 0))
    nwAux (Z :. 1 :. 0) = (True,  (0, 1))
    nwAux (Z :. i :. j) = (False, (i,  j))

    sw = fromFunction (Z :. n + 1 :. n + 1) swAux

    swAux (Z :. i :. 1) | i == n     = (True,  (1,     n))
    swAux (Z :. i :. 0) | i == n - 1 = (True,  (0, n - 1))
    swAux (Z :. i :. j)              = (False, (i,     j))

    ne = fromFunction (Z :. n + 1 :. n + 1) neAux

    neAux (Z :. 0 :. j) | j == n     = (True,  (n - 1, 0))
    neAux (Z :. 1 :. j) | j == n - 1 = (True,  (n,     1))
    neAux (Z :. i :. j)              = (False, (i,     j))

    se = fromFunction (Z :. n + 1 :. n + 1) seAux

    seAux (Z :. i :. j) | i == n - 1 && j == n     = (True,  (n - 1,     n))
    seAux (Z :. i :. j) | i == n     && j == n - 1 = (True,  (n,     n - 1))
    seAux (Z :. i :. j)                            = (False, (i,         j))

flatten :: Int -> Array D DIM2 a -> Array D DIM2 a
flatten innerN = R.reshape (Z :. (innerN :: Int) :. (1 :: Int))

mkJacobiMat :: Int -> Array D DIM2 Double
mkJacobiMat n = withInt $
                withBnd wr $
                withBnd er $
                withBnd sr $
                withBnd nr $
                corners

  where

    withInt cs = foldl' (R.++) cs [ flatten innerN $ ip k l | k <- [1..n - 3], l <- [1..n - 3 ] ]

    withBnd bndFn cs = foldl' (R.++) cs (P.map (flatten innerN) $ P.map bndFn [1..n - 3])

    corners = flatten innerN nw R.++
              flatten innerN ne R.++
              flatten innerN sw R.++
              flatten innerN se

    innerN = (n - 1)^2

    nw = fromFunction (Z :. n - 1 :. n -1) nwAux

    nwAux (Z :. 0 :. 1) =  1.0
    nwAux (Z :. 0 :. 0) = -4.0
    nwAux (Z :. 1 :. 0) =  1.0
    nwAux _             =  0.0

    ne = fromFunction (Z :. n - 1 :. n -1) neAux

    neAux (Z :. 0 :. j) | j == n - 3 =  1.0
    neAux (Z :. 0 :. j) | j == n - 2 = -4.0
    neAux (Z :. 1 :. j) | j == n - 2 =  1.0
    neAux _                          =  0.0

    sw = fromFunction (Z :. n - 1 :. n -1) swAux

    swAux (Z :. i :. 0) | i == n - 3 =  1.0
    swAux (Z :. i :. 0) | i == n - 2 = -4.0
    swAux (Z :. i :. 1) | i == n - 2 =  1.0
    swAux _                          =  0.0

    se = fromFunction (Z :. n - 1 :. n -1) seAux

    seAux (Z :. i :. j) | i == n - 3 && j == n - 2 =  1.0
    seAux (Z :. i :. j) | i == n - 2 && j == n - 2 = -4.0
    seAux (Z :. i :. j) | i == n - 2 && j == n - 3 =  1.0
    seAux _                                        =  0.0

    nr l = fromFunction (Z :. n - 1 :. n -1) (nrAux l)

    nrAux l (Z :. 0 :. j) | j == l     = -4.0
    nrAux l (Z :. 0 :. j) | j == l - 1 =  1.0
    nrAux l (Z :. 0 :. j) | j == l + 1 =  1.0
    nrAux l (Z :. 1 :. j) | j == l     =  1.0
    nrAux _ _                          =  0.0

    sr l = fromFunction (Z :. n - 1 :. n -1) (srAux l)

    srAux l (Z :. i :. j) | i == n - 2 && j == l     = -4.0
    srAux l (Z :. i :. j) | i == n - 2 && j == l - 1 =  1.0
    srAux l (Z :. i :. j) | i == n - 2 && j == l + 1 =  1.0
    srAux l (Z :. i :. j) | i == n - 3 && j == l     =  1.0
    srAux _ _                                        =  0.0

    er l = fromFunction (Z :. n - 1 :. n -1) (erAux l)

    erAux l (Z :. i :. j) | i == l     && j == n - 2 = -4.0
    erAux l (Z :. i :. j) | i == l - 1 && j == n - 2 =  1.0
    erAux l (Z :. i :. j) | i == l + 1 && j == n - 2 =  1.0
    erAux l (Z :. i :. j) | i == l     && j == n - 3 =  1.0
    erAux _ _                                        =  0.0

    wr l = fromFunction (Z :. n - 1 :. n -1) (wrAux l)

    wrAux l (Z :. i :. j) | i == l     && j == 0 = -4.0
    wrAux l (Z :. i :. j) | i == l - 1 && j == 0 =  1.0
    wrAux l (Z :. i :. j) | i == l + 1 && j == 0 =  1.0
    wrAux l (Z :. i :. j) | i == l     && j == 1 =  1.0
    wrAux _ _                                    =  0.0

    ip k l = fromFunction (Z :. n - 1 :. n -1) (ipAux k l)

    ipAux k l (Z :. i :. j) | i == k     && j == l     = -4.0
    ipAux k l (Z :. i :. j) | i == k - 1 && j == l     =  1.0
    ipAux k l (Z :. i :. j) | i == k + 1 && j == l     =  1.0
    ipAux k l (Z :. i :. j) | i == k     && j == l - 1 =  1.0
    ipAux k l (Z :. i :. j) | i == k     && j == l + 1 =  1.0
    ipAux _ _ _                                        =  0.0

mkJacobiVars :: Int -> Doc
mkJacobiVars n =
  vcat $
  punctuate (space <> text "\\\\")
            [ braces ( text "\\color" <>
                       braces (text "blue") <>
                       braces (text "u_" <> braces (int i <> int j))
                     )
            | i <- [1..n - 1], j <- [1..n - 1]
            ]

class Tex a where
  tex :: a -> Doc

instance Tex Double where
  tex = pPrint

instance (Source t a, Tex a) => Tex (Array t DIM1 a) where
 tex a = hcat $ punctuate (space <> text "&" <> space) elems
   where
     elems = [ tex (a!j) | i <- [0..n-1], let j = Z:. i ]
     Z :. n = extent a

instance (Source t a, Tex a) => Tex (Array t DIM2 a) where
 tex a = vcat $ punctuate (space <> text "\\\\") elems
   where
     elems = [ tex (slice a j) | i <- [0..n-1], let j = Any :. i :. All]
     Z :. n :. _m = extent a

matrix :: Int -> String
matrix n =  render $
            vcat [ text "\n\\begin{bmatrix}"
                 , tex $ transpose $ mkJacobiMat n
                 , text "\\end{bmatrix}"
                 , text "\\begin{bmatrix}"
                 , mkJacobiVars n
                 , text "\\end{bmatrix}"
                 , text "="
                 , text "\\begin{bmatrix}"
                 , vcat $ punctuate (space <> text "\\\\") $ mkJacobiBnd fn1a fn2a n
                 , text "\\end{bmatrix}\n"
                 ]


