{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Math.RK

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

-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  -- rkFixMain
  rkAdpMain
