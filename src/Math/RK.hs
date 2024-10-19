-- Stealing work I did on Hecate because I like it.

module Math.RK where

import Control.Monad.Trans.State (State, runState, state)
import Data.List (find)

-- Types -----------------------------------------------------------------------

data SystemState = SystemState {x :: Double, t :: Double} deriving (Eq, Show)

newtype EOM = EOM (SystemState -> Double) -- System Dynamics = derivative of x

data ButcherTable = ButcherTable
  { a :: Int -> Int -> Double,
    b :: Int -> Double,
    c :: Int -> Double,
    s :: Int,
    o :: Int
  }

newtype Stepper = Stepper {step :: Double -> SystemState -> SystemState}

data AdpButcherTable = AdpButcherTable {bt :: ButcherTable, b' :: Int -> Double}

newtype ErrStepper = ErrStepper {errStep :: Double -> SystemState -> (SystemState, Double)}

newtype AdpStepper = AdpStepper (State SystemState AdpStepper)

-- Fixed-Step RK4 Verbatim from Wikipedia --------------------------------------

rk4 :: EOM -> Double -> SystemState -> SystemState
rk4 (EOM f) h (SystemState x0 t0) = SystemState {x = x1, t = t1}
  where
    k1 = f $ SystemState x0 t0
    k2 = f $ SystemState (x0 + h * k1 / 2) (t0 + h / 2)
    k3 = f $ SystemState (x0 + h * k2 / 2) (t0 + h / 2)
    k4 = f $ SystemState (x0 + h * k3) (t0 + h)
    x1 = x0 + h * (k1 + 2 * k2 + 2 * k3 + k4) / 6
    t1 = t0 + h

-- General Fixed-Step RK -------------------------------------------------------

rk :: ButcherTable -> EOM -> Double -> SystemState -> SystemState
rk (ButcherTable a b c s _) (EOM f) h ss@(SystemState x0 t0) =
  SystemState
    { x = x0 + h * sum [b i * k i | i <- [0 .. s - 1]],
      t = t0 + h
    }
  where
    k 0 = f ss
    k n =
      f $
        SystemState
          { x = x0 + h * sum [k i * a n i | i <- [0 .. n - 1]],
            t = t0 + h * c n
          }

-- Butcher Table Utility -------------------------------------------------------

mkButcherTable :: [[Double]] -> [Double] -> [Double] -> Int -> Int -> ButcherTable
mkButcherTable a b c s o =
  ButcherTable
    { a = (!!) <$> (!!) a,
      b = (!!) b,
      c = (!!) c,
      s = s,
      o = o
    }

-- Some Common Butcher Tables --------------------------------------------------

btRK4 =
  mkButcherTable
    [[], [1 / 3], [-1 / 3, 1], [1, -1, 1]]
    [1 / 8, 3 / 8, 3 / 8, 1 / 8]
    [0, 1 / 3, 2 / 3, 1]
    4
    4

btRKF =
  mkButcherTable
    [ [],
      [1 / 4],
      [3 / 32, 9 / 32],
      [1932 / 2197, -7200 / 2197, 7296 / 2197],
      [439 / 216, -8, 3680 / 513, -845 / 4104],
      [-8 / 27, 2, -3544 / 2565, 1859 / 4104, -11 / 40]
    ]
    [16 / 135, 0, 6656 / 12825, 28561 / 56430, -9 / 50, 2 / 55]
    [0, 1 / 4, 3 / 8, 12 / 13, 1, 1 / 2]
    6
    5

btDOPRI =
  mkButcherTable
    [ [],
      [1 / 5],
      [3 / 40, 9 / 40],
      [44 / 45, -56 / 15, 32 / 9],
      [19372 / 6561, -25360 / 2187, 64448 / 6561, -212 / 729],
      [9017 / 3168, -355 / 33, 46732 / 5247, 49 / 176, -5103 / 18656],
      [35 / 384, 0, 500 / 1113, 125 / 192, -2187 / 6784, 11 / 84]
    ]
    [35 / 384, 0, 500 / 1113, 125 / 192, -2187 / 6784, 11 / 84, 0]
    [0, 1 / 5, 3 / 10, 4 / 5, 8 / 9, 1, 1]
    7
    5

-- Multiple Fixed-Size Steps ---------------------------------------------------

mkRKStepper :: ButcherTable -> EOM -> Stepper
mkRKStepper bt eom = Stepper {step = rk bt eom}

-- | Infinite steps
multiFixedStep :: Stepper -> Double -> SystemState -> [SystemState]
multiFixedStep stp h s0 = s0 : iterate (step stp h) s0

-- General Error-Estimating RK -------------------------------------------------

rkErr :: AdpButcherTable -> EOM -> Double -> SystemState -> (SystemState, Double)
rkErr (AdpButcherTable (ButcherTable a b c s o) b') eom h s0 = (SystemState x1 t1, x2 - x1)
  where
    (SystemState x1 t1) = rk (ButcherTable a b c s o) eom h s0
    (SystemState x2 t2) = rk (ButcherTable a b' c s (o - 1)) eom h s0

-- Error-Estimating Butcher Table Utility --------------------------------------

mkAdpButcherTable :: ButcherTable -> [Double] -> AdpButcherTable
mkAdpButcherTable bt b' = AdpButcherTable {bt = bt, b' = (!!) b'}

-- Common Error-Estimating Butcher Tables --------------------------------------

btaRKF =
  mkAdpButcherTable
    btRKF
    [ 25 / 216,
      0,
      1408 / 2565,
      2197 / 4104,
      -1 / 5,
      0
    ]

btaDOPRI =
  mkAdpButcherTable
    btDOPRI
    [ 5179 / 57600,
      0,
      7571 / 16695,
      393 / 640,
      -92097 / 339200,
      187 / 2100,
      1 / 40
    ]

-- Adaptive Size Stepper -------------------------------------------------------

mkRKAdpStepper :: Double -> AdpButcherTable -> EOM -> AdpStepper
mkRKAdpStepper absTol abt@(AdpButcherTable (ButcherTable _ _ _ _ order) _) eom = AdpStepper (state $ go 1)
  where
    -- This error stepper gets embedded into the adaptive stepper via the g function
    rke = rkErr abt eom

    -- Uses the algorithm from (https://en.wikipedia.org/wiki/Adaptive_step_size) to adapt the step size.
    g :: SystemState -> (SystemState, Double, Double) -> (SystemState, Double, Double)
    g s0 (_, h, _) = (s', h', e')
      where
        (s', e') = rke h s0
        bigE = abs (e' / absTol)
        -- NOTE: Did not realize that ^ only worked for integral powers.
        h' = h * (1 / bigE) ** (1 / fromIntegral order)

    -- This function computes a forward step that satisfies the requested tolerance and uses that information to create a new stepper containing a best guess step size.
    -- Used to bootstrap the State monad with some initial guess step size h0.
    go :: Double -> SystemState -> (AdpStepper, SystemState)
    go h0 s0@(SystemState _ t0) = (AdpStepper (state $ go h), s1)
      where
        -- Get an answer that satisfies the tolerance
        Just (s1, h, err) = find (\(_, _, err) -> abs err <= absTol) (tail $ iterate (g s0) (undefined, h0, undefined))

adpStep :: AdpStepper -> SystemState -> SystemState
adpStep (AdpStepper st) s0 = s1
  where
    (_, s1) = runState st s0

-- | Infinite Steps
multiAdpStep :: AdpStepper -> SystemState -> [SystemState]
multiAdpStep (AdpStepper st) s0 = s0 : multiAdpStep st' s1
  where
    (st', s1) = runState st s0
