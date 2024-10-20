{-# LANGUAGE DataKinds #-}

-- https://blog.fmap.fr/posts/karplus-strong-sound-synthesis.html

module Noise where

import Control.Monad
import Data.List (unfoldr)
import qualified Deque.Strict as DQ
import LambdaSound
import System.Random

--------------------------------------------------------------------------------
-- My implementation from reading ----------------------------------------------
--------------------------------------------------------------------------------

-- Guitar Strum ----------------------------------------------------------------

-- Karplus-Strong Algorithm
ksGuitar :: (Fractional a) => [a] -> [a]
ksGuitar seed = seed ++ go (ksGuitar seed)
  where
    go (x : y : rest) = 0.995 * (x + y) / 2 : go (y : rest)

mkGuitar :: Int -> Int -> IO [Double]
mkGuitar sampRate hz = ksGuitar <$> seed
  where
    seedLength = round $ fromIntegral sampRate / fromIntegral hz
    seed = replicateM seedLength ((0.5 -) <$> randomIO)

-- Snare Hit -------------------------------------------------------------------

ksSnare :: (Fractional a) => [a] -> IO [a]
ksSnare seed = (seed ++) <$> tail
  where
    go (x : y : rest) = do
      b <- randomIO
      let fac = if b then 1 else -1
      r <- go (y : rest)
      return (fac * 0.995 * (x + y) / 2 : r)

    tail = ksSnare seed >>= go

mkSnare :: Int -> IO [Double]
mkSnare len = ksSnare (replicate len 0.5)

--------------------------------------------------------------------------------
-- Blog Implementation ---------------------------------------------------------
--------------------------------------------------------------------------------

karplusStrong ::
  -- | Initializer
  (StdGen -> Maybe (Pulse, StdGen)) ->
  -- | Slider
  (StdGen -> Pulse -> Pulse -> (Pulse, StdGen)) ->
  -- | Sample Rate
  Int ->
  -- | Instrument
  (Hz -> Sound 'I Pulse)
karplusStrong genPulse f sampleRate freq = unfoldrSoundPulse slide (dq0, gen)
  where
    gen = mkStdGen 142
    waveTableLen = floor (fromIntegral sampleRate / freq)
    waveTable = take waveTableLen $ unfoldr genPulse gen
    dq0 = DQ.fromConsAndSnocLists waveTable []
    slide (dq, g) = case DQ.uncons dq of
      Just (a, as) | Just a' <- DQ.head as -> let (newA, g') = f g a a' in (a, (DQ.snoc newA as, g'))
      _ -> error "silence"

noisyMain :: IO ()
noisyMain = do
  let sampleRate = 44100

      attenuate a b = 0.995 * (a + b) / 2
      centeredPulse g = case randomR (-0.5, 0.5) g of
        (a, g') -> Just (Pulse a, g')

      -- guitar
      guitar = karplusStrong centeredPulse (\_gen a a' -> (attenuate a a', _gen)) sampleRate

      -- snare
      snare =
        karplusStrong
          (\g -> Just (0.5, g))
          ( \gen a a' -> case random gen of
              (b, gen') ->
                let v = attenuate a a'
                 in (if b then v else negate v, gen')
          )
          sampleRate

      -- guitar sounds
      gtr1 = setDuration 2 $ asNote guitar a3
      gtr2 = simpleReverb 0.01 gtr1
      gtr3 = setDuration 2 $ parallel [asNote guitar x | x <- [c3, e3, g3]]
      gtr4 = simpleReverb 0.01 gtr3

      -- drum sound
      drm = setDuration 0.3 $ asNote snare a3

      -- song
      song =
        let gtrLoop =
              [ ([c3, g3], [c4, d4 + 1]),
                ([a2 + 1, g3], [c4, d4 + 1]),
                ([g2 + 1, f3], [g4, f4]),
                ([f2, d3 + 1], [f4, d4 + 1])
              ]
            gtr =
              repeatSound 2 $
                sequentially
                  [ repeatSound 4 $
                      parallel
                        [ setDuration 0.9 (parallel [asNote guitar x | x <- l]),
                          setDuration 0.3 silence >>> sequentially (map (setDuration 0.3 . asNote guitar) r)
                        ]
                    | (l, r) <- gtrLoop
                  ]
            dm1 = repeatSound 48 (setDuration 0.6 drm)
            dm2 = repeatSound 96 (reduce 0.8 drm)
            dm3 = repeatSound 32 (amplify 1.2 $ setDuration 0.75 silence >>> setDuration 0.15 drm)
         in setDuration 2 silence >>> parallel [gtr, dm1, dm2, dm3]

      allSounds = [gtr1, gtr2, gtr3, gtr4, drm, song]

  mapM_ (play sampleRate 1) allSounds

-- sequence_
--   [ saveWav
--       ("karplus_strong_" ++ show n ++ ".wav")
--       (fromIntegral sampleRate)
--       s
--     | (n, s) <- zip [(1 :: Int) ..] allSounds
--   ]
