-- "Normal" solution by Bird and Wadler

-- I would personally prefer a solution involving rotation calculations...

module TLII.BirdWadler where

import Prelude (and, print, zip, ($), (/=))

-- Cube is [up, front, right, bottom, left, down]

cubes = ["BGWGBR", "WGBWRR", "GWRBRR", "BRGGWW"]

rot [u, f, r, b, l, d] = [u, r, b, l, f, d]

twist [u, f, r, b, l, d] = [f, r, u, l, d, b]

flip [u, f, r, b, l, d] = [d, l, b, r, f, u]

orientations c =
  [ c'''
    | c' <- [c, rot c, rot $ rot c, rot $ rot $ rot c],
      c'' <- [c', twist c', twist $ twist c'],
      c''' <- [c'', flip c'']
  ]

visible [u, f, r, b, l, d] = [f, r, b, l]

compatible c c' = and [x /= x' | (x, x') <- zip (visible c) (visible c')]

allowed c cs = and [compatible c c' | c' <- cs]

solutions [] = [[]]
solutions (c : cs) =
  [ c' : cs'
    | cs' <- solutions cs,
      c' <- orientations c,
      allowed c' cs
  ]

main = print $ solutions cubes
