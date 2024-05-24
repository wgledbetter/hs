-- http://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial

module MT.Repa where

import Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V

x = fromListUnboxed (Z :. (3 :: Int) :. (3 :: Int) :. (3 :: Int)) [1 .. 27] :: Array U DIM3 Int

ex = extent x

-- N Dims
r = rank ex

-- Total Elements
sz = size ex

-- Flatten to vector
fv = toUnboxed x

-- Initialization --------------------------------------------------------------
inputs = [1 .. 10] :: [Double]

x1 = fromListUnboxed (Z :. 10 :: DIM1) inputs

x2 = fromListUnboxed (Z :. (5 :: Int) :. (2 :: Int)) inputs

x2' = fromListUnboxed (Z :. 5 :. 2 :: DIM2) inputs

-- From Vector
xx1 = fromUnboxed (Z :. (10 :: Int)) (V.enumFromN 0 10 :: V.Vector Double)

xx2 = fromUnboxed (Z :. (3 :: Int) :. (3 :: Int)) (V.enumFromN 0 9 :: V.Vector Double)

-- Indexing --------------------------------------------------------------------

idxVal = xx1 ! (Z :. 2)

idxVal' = xx2 ! (Z :. 1 :. 1)

-- Operations ------------------------------------------------------------------

xSquare = R.map (^ 2) x1 -- Delayed array

sqVal = xSquare ! (Z :. 0) -- Invokes delayed computation
