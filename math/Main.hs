module Main where

import Data.List (elemIndex, find)
import Math.Opt (bisect)
import Math.RK

-- Integrate to a specific state -----------------------------------------------

data GenStepper = GenStepper (Double -> SystemState -> SystemState) Double

genFromAdpStepper :: AdpStepper -> GenStepper
genFromAdpStepper adp = GenStepper (const $ adpStep adp) undefined

genFromFixStepper :: Stepper -> Double -> GenStepper
genFromFixStepper stp h = GenStepper (stepH stp) h

findZero :: Int -> Double -> (SystemState -> Double) -> Stepper -> SystemState -> Maybe SystemState
findZero maxSearchLength tol f stp s0@(SystemState x0 t0) =
  let steps = iterate (step stp) s0
      f0 = f $ head steps
      mbOpposite = findWithin maxSearchLength (\x -> signum (f x) /= signum f0) steps
   in do
        opposite@(SystemState _ tF) <- mbOpposite

        let Just farIdx = elemIndex opposite steps
            near@(SystemState xN tN) = steps !! (farIdx - 1)

            h0 = tF - tN
            hStar = bisect tol (\h -> f (stepH stp h near)) (0, h0)
            sStar = stepH stp hStar near

        return sStar

findWithin :: Int -> (a -> Bool) -> [a] -> Maybe a
findWithin n f l = find f $ take n l

-- Examples --------------------------------------------------------------------

rkFixMain :: IO ()
rkFixMain = do
  putStrLn "Single Fixed Step"
  let f (SystemState x t) = x -- Result of integration will be x = e^t
      h = 1
      ic = SystemState {x = 1, t = 0}
      eom = EOM f

      ss1 = rk4 eom h ic
      ss2 = rk btRK4 eom h ic
      ss3 = rk btRKF eom h ic
      ss4 = rk btDOPRI eom h ic

  print ss1
  print ss2
  print ss3
  print ss4
  putStrLn "\n"

rkAdpMain :: IO ()
rkAdpMain = do
  putStrLn "Single Adaptive Step"
  let f (SystemState x t) = x
      h = 1
      ic = SystemState {x = 1, t = 0}
      eom = EOM f
      absTol = 1e-10

      astRKF = mkRKAdpStepper absTol btaRKF eom
      astDOPRI = mkRKAdpStepper absTol btaDOPRI eom

      ss1 = adpStep astRKF ic
      ss2 = adpStep astDOPRI ic

  print ss1
  print ss2
  putStrLn "\n"

  putStrLn "Multiple Adaptive Steps"
  let rkfSteps = multiAdpStep astRKF ic
      dopriSteps = multiAdpStep astDOPRI ic

  print $ take 10 rkfSteps
  print $ take 10 dopriSteps
  putStrLn "\n"

zeroMain :: IO ()
zeroMain = do
  putStrLn "Root Finding - Fixed Stepper:"
  let f (SystemState x t) = x
      h = 0.01
      ic = SystemState {x = 1, t = 0}
      eom = EOM f
      cond (SystemState x t) = t - 2.5 -- t = 2.5 implies solving for e^2.5
      stp = mkRKStepper btDOPRI eom h

      z = findZero 1000 1e-10 cond stp ic

  print z
  putStrLn "\n"

  putStrLn "Root Finding - Adaptive Stepper:"
  let (AdpStepper _ stp) = mkRKAdpStepper 1e-10 btaDOPRI eom

      z = findZero 10000 1e-8 cond stp ic

  print z

-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  -- rkFixMain
  -- rkAdpMain
  zeroMain
