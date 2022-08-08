{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- Implementing and measuring examples from https://www.youtube.com/watch?v=AzJVFkm42zM

module Scratch.BTH1 where

import Language.Haskell.TH (Code, Quote)

-- Power Example ---------------------------------------------------------------

power5 :: Int -> Int
power5 k = k * k * k * k * k

power :: Int -> Int -> Int
power 0 k = 1
power n k = k * power (n - 1) k

power5' :: Int -> Int
power5' = power 5

tPower :: (Quote m) => Int -> Code m (Int -> Int)
tPower 0 = [||\k -> 1||]
tPower n = [||\k -> k * $$(tPower (n - 1)) k||]
