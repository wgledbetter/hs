-- http://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial

module MT.Repa where

import Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V
import HB.Ch22 (Reader (Reader), runReader)

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

-- Evaluating delayed arrays
-- The "computeP" and "computeS" are parallel and sequential functions to compute values of a delayed array.
-- The type is kind of weird, though.
-- They return a monad of an array, not just the array itself: `m (Array r2 sh e)`.
-- According to the docs, this is to enforce a "notion of sequence" so that all values are computed before the next step.
-- Something about preventing nested data parallelism.
-- Anyway, you just have to wrap the output in some type that has a Monad instance

compIO = computeP xSquare :: IO (Array U DIM1 Double)

compL = computeP xSquare :: [Array U DIM1 Double]

-- That feels weird. I don't like it.

foldReader = foldP (+) 0 xSquare :: Reader Int (Array U DIM0 Double)

folded = runReader foldReader 69420

-- Fold reduces dimension by 1
sumCols = foldP (+) 0 (fromListUnboxed (Z :. 3 :. 3 :: DIM2) [0 .. 8]) :: Reader String (Array U DIM1 Double)

colSum = runReader sumCols "Gimme that"

-- foldAll reduces to scalar
sumAll = foldAllP (+) 0 (fromListUnboxed (Z :. 2 :. 2 :. 2 :: DIM3) [0 .. 7]) :: Reader String Int

allSum = runReader sumAll "answer pls"

-- traverse, but not Traversable
xTrav :: Array U DIM3 Int
xTrav = fromListUnboxed (Z :. (3 :: Int) :. (3 :: Int) :. (3 :: Int)) [1 .. 27]

xT1 :: Array D DIM3 Int
xT1 =
  R.traverse
    xTrav -- Source Array
    id -- Source/Dest index mapper (sh -> sh')
    (\_ (Z :. i :. j :. k) -> i) -- Map from input to output ((sh -> a) -> sh' -> b)

getValsFromDelayed :: (Shape sh, V.Unbox v) => Array D sh v -> Array U sh v
getValsFromDelayed a = runReader (computeP a) "Gimme"

xT2 =
  R.traverse
    xTrav
    id -- Shape remains the same
    (\inputAccessor (Z :. i :. j :. k) -> inputAccessor (Z :. j :. k :. i)) -- Value at out(i,j,k) = input(j,k,i)

xT3 =
  R.traverse
    xTrav
    (\(bigDim :. lastDim) -> bigDim) -- Reduce dimension by one
    (\inputAccessor outputIndex -> inputAccessor (outputIndex :. 0)) -- Hard code zero index on dropped dimension

-- Elementwise

xxx = xT1 +^ xT2
