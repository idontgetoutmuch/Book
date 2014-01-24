{-# LANGUAGE FlexibleInstances   #-}

module Chap1Aux where

import Prelude         as P
import Data.Array.Repa as R
import Data.List ( foldl' )
import Text.PrettyPrint.HughesPJClass

mkJacobiMat :: Int -> Array D DIM2 Double
mkJacobiMat n = withInt $
                withBnd wr $
                withBnd er $
                withBnd sr $
                withBnd nr $
                corners

  where

    withInt cs = foldl' (R.++) cs [ flatten $ ip k l | k <- [1..n - 3], l <- [1..n - 3 ] ]

    withBnd bndFn cs = foldl' (R.++) cs (P.map flatten $ P.map bndFn [1..n - 3])

    corners = flatten nw R.++
              flatten ne R.++
              flatten sw R.++
              flatten se

    flatten :: Array D DIM2 Double -> Array D DIM2 Double
    flatten = R.reshape (Z :. (innerN :: Int) :. (1 :: Int))

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

matrix5 =  render $
           vcat [ text "\n\\begin{bmatrix}"
                , tex $ mkJacobiMat 5
                , text "\\end{bmatrix}\n"
                ]

main = do
  writeFile "matrix5.tex" matrix5

