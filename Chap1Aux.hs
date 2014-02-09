{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wall                      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind   #-}
{-# OPTIONS_GHC -fno-warn-missing-methods  #-}
{-# OPTIONS_GHC -fno-warn-orphans          #-}

module Chap1Aux (
    pPrint
  , mkJacobiMat
  , mkJacobiBnd
  , mkJacobiVars
  , matrixTex
  ) where

import Prelude         as P
import Data.Array.Repa as R
import Data.List ( foldl' )
import Text.PrettyPrint.HughesPJClass
import PrettyPrint ()

fn1a :: Int -> Doc
fn1a = int

fn2a :: Int -> [(Int, Int)] -> Doc
fn2a _ = hcat .
         punctuate (text " + ") .
         P.map (\(i, j) -> text "-" <>
                           braces ( text "\\color" <>
                                    braces (text "red") <>
                                    braces (text "u_" <> braces (int i <> int j))
                                  )
               )

mkJacobiBnd :: (Int -> a) -> (Int -> [(Int, Int)] -> a) -> Int -> [a]
mkJacobiBnd fn1 fn2 n = nAndNw P.++ lowerRows

  where

    nAndNw =  [pp nw] P.++
              edge north P.++
              [pp ne]

    sAndSw = [pp sw] P.++
             edge south P.++
             [pp se]

    lowerRows = foldl'
                (\s m -> take 1 (drop m (edge west)) P.++
                         take (n - 3) (drop (m * (n - 3)) inners) P.++
                         take 1 (drop m (edge east)) P.++
                         s)
                sAndSw
                (reverse [0..n - 4])

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
mkJacobiMat n =
  traverse (mkMatOfMat n)
  (\(Z :. i :. j :. k :. l) ->
    (Z :. j + i * (n - 2) :. l + k * (n - 2)))
  (\f (Z :. i :. j) ->
    f (Z :. i `div` (n - 1) :. i `mod` (n - 1) :. j `div` (n - 1) :. j `mod` (n - 1)))

mkMatOfMat :: Int -> Array D DIM4 Double
mkMatOfMat n = fromFunction (Z :. n - 1 :. n - 1 :. n - 1 :. n - 1) (mkMat4Elem n)

mkMat4Elem :: Int -> DIM4 -> Double
mkMat4Elem = go
  where

    go n (Z :. i :. j :. k :. l) = (mkMatAuxil n (Z :. i :. j :. k :. l))!(Z :. k :. l)

    mkMatAuxil n (Z :. i :. j :. _ :. _) | i == 0 &&
                                           j == 0     = nw n
    mkMatAuxil n (Z :. i :. j :. _ :. _) | i == 0 &&
                                           j >= 1 &&
                                           j <= n - 3 = nr n j
    mkMatAuxil n (Z :. i :. j :. _ :. _) | i == 0 &&
                                           j == n - 2 = ne n

    mkMatAuxil n (Z :. i :. j :. _ :. _) | i >= 1 &&
                                           i <= n - 3 &&
                                           j == 0     = wr n i
    mkMatAuxil n (Z :. i :. j :. _ :. _) | i >= 1 &&
                                           i <= n - 3 &&
                                           j >= 1 &&
                                           j <= n - 3 = ip n i j
    mkMatAuxil n (Z :. i :. j :. _ :. _) | i >= 1 &&
                                           i <= n - 3 &&
                                           j == n - 2 = er n i

    mkMatAuxil n (Z :. i :. j :. _ :. _) | i == n - 2 &&
                                           j == 0     = sw n
    mkMatAuxil n (Z :. i :. j :. _ :. _) | i == n - 2 &&
                                           j >= 1 &&
                                           j <= n - 3 = sr n j
    mkMatAuxil n (Z :. i :. j :. _ :. _) | i == n - 2 &&
                                           j == n - 2 = se n

    mkMatAuxil n (Z :. i :. j :. _ :. _)              = ip n i j

    ip n k l = fromFunction (Z :. n - 1 :. n -1) (ipAux k l)

    ipAux k l (Z :. i :. j) | i == k     && j == l     = -4.0
    ipAux k l (Z :. i :. j) | i == k - 1 && j == l     =  1.0
    ipAux k l (Z :. i :. j) | i == k + 1 && j == l     =  1.0
    ipAux k l (Z :. i :. j) | i == k     && j == l - 1 =  1.0
    ipAux k l (Z :. i :. j) | i == k     && j == l + 1 =  1.0
    ipAux _ _ _                                        =  0.0

    nw n = fromFunction (Z :. n - 1 :. n -1) nwAux

    nwAux (Z :. 0 :. 1) =  1.0
    nwAux (Z :. 0 :. 0) = -4.0
    nwAux (Z :. 1 :. 0) =  1.0
    nwAux _             =  0.0

    ne n = fromFunction (Z :. n - 1 :. n -1) (neAux n)

    neAux n (Z :. 0 :. j) | j == n - 3 =  1.0
    neAux n (Z :. 0 :. j) | j == n - 2 = -4.0
    neAux n (Z :. 1 :. j) | j == n - 2 =  1.0
    neAux _ _                          =  0.0

    sw n = fromFunction (Z :. n - 1 :. n -1) (swAux n)

    swAux n (Z :. i :. 0) | i == n - 3 =  1.0
    swAux n (Z :. i :. 0) | i == n - 2 = -4.0
    swAux n (Z :. i :. 1) | i == n - 2 =  1.0
    swAux _ _                          =  0.0

    se n = fromFunction (Z :. n - 1 :. n -1) (seAux n)

    seAux n (Z :. i :. j) | i == n - 3 && j == n - 2 =  1.0
    seAux n (Z :. i :. j) | i == n - 2 && j == n - 2 = -4.0
    seAux n (Z :. i :. j) | i == n - 2 && j == n - 3 =  1.0
    seAux _ _                                        =  0.0

    nr n l = fromFunction (Z :. n - 1 :. n -1) (nrAux l)

    nrAux l (Z :. 0 :. j) | j == l     = -4.0
    nrAux l (Z :. 0 :. j) | j == l - 1 =  1.0
    nrAux l (Z :. 0 :. j) | j == l + 1 =  1.0
    nrAux l (Z :. 1 :. j) | j == l     =  1.0
    nrAux _ _                          =  0.0

    sr n l = fromFunction (Z :. n - 1 :. n -1) (srAux n l)

    srAux n l (Z :. i :. j) | i == n - 2 && j == l     = -4.0
    srAux n l (Z :. i :. j) | i == n - 2 && j == l - 1 =  1.0
    srAux n l (Z :. i :. j) | i == n - 2 && j == l + 1 =  1.0
    srAux n l (Z :. i :. j) | i == n - 3 && j == l     =  1.0
    srAux _ _ _                                        =  0.0

    er n l = fromFunction (Z :. n - 1 :. n -1) (erAux n l)

    erAux n l (Z :. i :. j) | i == l     && j == n - 2 = -4.0
    erAux n l (Z :. i :. j) | i == l - 1 && j == n - 2 =  1.0
    erAux n l (Z :. i :. j) | i == l + 1 && j == n - 2 =  1.0
    erAux n l (Z :. i :. j) | i == l     && j == n - 3 =  1.0
    erAux _ _ _                                        =  0.0

    wr n l = fromFunction (Z :. n - 1 :. n -1) (wrAux l)

    wrAux l (Z :. i :. j) | i == l     && j == 0 = -4.0
    wrAux l (Z :. i :. j) | i == l - 1 && j == 0 =  1.0
    wrAux l (Z :. i :. j) | i == l + 1 && j == 0 =  1.0
    wrAux l (Z :. i :. j) | i == l     && j == 1 =  1.0
    wrAux _ _                                    =  0.0

mkJacobiVars :: Int -> Doc
mkJacobiVars n =
  vcat $
  punctuate (space <> text "\\\\")
            [ braces ( text "\\color" <>
                       braces (text "blue") <>
                       braces (text "u_" <> braces (int i <> int j))
                     )
            | j <- [1..n - 1], i <- [1..n - 1]
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

matrixTex :: Int -> String
matrixTex n =  render $
               vcat [ text "\n\\begin{bmatrix}"
                    , tex $ mkJacobiMat n
                    , text "\\end{bmatrix}"
                    , text "\\begin{bmatrix}"
                    , mkJacobiVars n
                    , text "\\end{bmatrix}"
                    , text "="
                    , text "\\begin{bmatrix}"
                    , vcat $ punctuate (space <> text "\\\\") $ mkJacobiBnd fn1a fn2a n
                    , text "\\end{bmatrix}\n"
                    ]
