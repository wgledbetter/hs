module FPPG.Four where

-- 1
fDiffFwdDeriv :: (Fractional a) => a -> (a -> a) -> a -> a
fDiffFwdDeriv dx f x = (f (x + dx) - f x) / dx

fDiffBckDeriv :: (Fractional a) => a -> (a -> a) -> a -> a
fDiffBckDeriv dx f x = (f x - f (x - dx)) / dx

fDiffCtrDeriv :: (Fractional a) => a -> (a -> a) -> a -> a
fDiffCtrDeriv dx f x = (f (x + dx) - f (x - dx)) / (2 * dx)

f1 :: (Floating a) => a -> a
f1 x = x ** 2 / 2

a10 :: Double -> Double
a10 = fDiffCtrDeriv 10 f1

a1 :: Double -> Double
a1 = fDiffCtrDeriv 1 f1

a01 :: Double -> Double
a01 = fDiffCtrDeriv 0.1 f1

-- 2
f2 :: Floating a => a -> a
f2 = (** 3)

df2 :: Floating a => a -> a
df2 x = 3 * x ** 2

diffF2 :: Double -> Double
diffF2 = fDiffCtrDeriv 1 f2

err2 :: Double -> Double
err2 x = diffF2 x - df2 x

err2pct :: Double -> Double
err2pct x = err2 x / df2 x

errAt4with :: Floating a => a -> a
errAt4with dt = (fDiffCtrDeriv dt f2 4 - df2 4) / df2 4

errAtWith :: Floating a => a -> a -> a
errAtWith x dt = (fDiffCtrDeriv dt f2 x - df2 x) / df2 x

onePctAt4 = 0.69282032302758

onePctAt1 = 0.173205080756890025
