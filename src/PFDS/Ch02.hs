module PFDS.Ch02 where

-- Stacks ----------------------------------------------------------------------

class Stack s where
  empty :: s a
  isEmpty :: s a -> Bool
  cons :: a -> s a -> s a
  head :: s a -> a
  tail :: s a -> s a

instance Stack [] where
  empty = []

  isEmpty [] = True
  isEmpty _ = False

  cons x xs = x : xs

  head (x : _) = x

  tail (_ : xs) = xs
