module Math.Dynamical where

import Data.Function (fix)
import qualified Data.Vector as V
import Memo (memoizeInt)

-- Utils -----------------------------------------------------------------------

ewSum :: (Num a) => V.Vector a -> V.Vector a -> V.Vector a
ewSum x y = V.zipWith (+) x y

scalMult :: (Num a) => a -> V.Vector a -> V.Vector a
scalMult v x = V.map (* v) x

-- Dynamics Class --------------------------------------------------------------

class Dynamics d where
  numXVars :: d -> Int
  numUVars :: d -> Int
  ode :: d -> V.Vector Double -> V.Vector Double

  stateOf :: d -> V.Vector Double -> V.Vector Double
  stateOf dyn x = V.take (numXVars dyn) x

  timeOf :: d -> V.Vector Double -> Double
  timeOf dyn x = x V.! (numXVars dyn)

  ctrlOf :: d -> V.Vector Double -> V.Vector Double
  ctrlOf dyn x = V.drop (numXVars dyn + 1) x

-- Dynamics Implementations ----------------------------------------------------

data TwoD = TwoD Double -- Velocity Magnitude
  deriving (Eq, Show)

instance Dynamics TwoD where
  numXVars = const 2
  numUVars = const 1
  ode (TwoD vMag) x0 = V.fromList [vMag * cos theta, vMag * sin theta]
    where
      theta = x0 V.! 3

data OneD = OneD Double -- Acceleration Magnitude
  deriving (Eq, Show)

instance Dynamics OneD where
  numXVars = const 2
  numUVars = const 0
  ode (OneD accMag) x0 = V.fromList [x0 V.! 1, accMag]

-- Integrator ------------------------------------------------------------------

data ButcherTableau = ButcherTableau
  { stages :: Int,
    bTabA :: [[Double]],
    bTabT :: [Double],
    bTabB :: [Double],
    bTabC :: [Double]
  }

euler =
  ButcherTableau
    { stages = 1,
      bTabA = [[]],
      bTabT = [0],
      bTabB = [1],
      bTabC = [0]
    }

midpoint =
  ButcherTableau
    { stages = 2,
      bTabA = [[], [1 / 2]],
      bTabT = [0, 1 / 2],
      bTabB = [0, 1],
      bTabC = [0, 0]
    }

rkf45 =
  ButcherTableau
    { stages = 6,
      bTabA =
        [ [],
          [1 / 4],
          [3 / 32, 9 / 32],
          [1932 / 2197, -7200 / 2197, 7296 / 2197],
          [439 / 216, -8, 3680 / 513, -845 / 4104],
          [-8 / 27, 2, -3544 / 2565, 1859 / 4104, -11 / 40]
        ],
      bTabB = [16 / 135, 0, 6656 / 12825, 28561 / 56430, -9 / 50, 2 / 55],
      bTabC = [25 / 216, 0, 1408 / 2565, 2197 / 4104, -1 / 5, 0],
      bTabT = [0, 1 / 4, 3 / 8, 12 / 13, 1, 1 / 2]
    }

rkStep :: (Dynamics d) => ButcherTableau -> d -> Double -> V.Vector Double -> V.Vector Double
rkStep bt dyn dt stc =
  ewSum
    x0
    ( scalMult
        dt
        ( foldr
            ewSum
            (V.fromList [0 :: Double | _ <- [0 .. numXVars dyn]])
            [ scalMult
                (bTabB bt !! i)
                (memK i)
              | i <- [0 .. stages bt - 1]
            ]
        )
    )
  where
    f = ode dyn
    x0 = stateOf dyn stc
    t0 = timeOf dyn stc
    c0 = ctrlOf dyn stc

    memK = fix (memoizeInt . k)

    k _ 0 = f stc
    k g i = f (xx V.++ V.fromList [tt] V.++ c0)
      where
        xx =
          ewSum
            x0
            ( scalMult
                dt
                ( foldr
                    ewSum
                    (V.fromList [0 :: Double | _ <- [0 .. numXVars dyn]])
                    [ scalMult
                        (bTabA bt !! i !! j)
                        (g j)
                      | j <- [0 .. i - 1]
                    ]
                )
            )
        tt = t0 + dt * bTabT bt !! i
