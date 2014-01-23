{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances   #-}

module PrettyPrint where

import Data.Array.Repa

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

instance (Source t a, Pretty a) => Pretty (Array t DIM1 a) where
 pPrint a = brackets $ hcat $ punctuate (comma <> space) elems
   where
     elems = [ pPrint (a!j) | i <- [0..n-1], let j = Z:. i ]
     Z :. n = extent a

instance (Source t a, Pretty a) => Pretty (Array t DIM2 a) where
 pPrint a = vcat elems
   where
     elems = [ pPrint (slice a j) | i <- [0..n-1], let j = Any :. i :. All]
     Z :. n :. _m = extent a
