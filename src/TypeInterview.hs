{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- https://aphyr.com/posts/342-typing-the-technical-interview
-- All comments are my own

module TypeInterview where

-- Dunno why we need this
-- OOOOOOOOOOOOOOOOOOHHHHH!!!!
nil = undefined

-- List ------------------------------------------------------------------------

data Nil

data Cons x xs

-- First -----------------------------------------------------------------------

-- This sets a pattern of multi-param typeclasses where the last parameter is effectively the return value.
-- At this declaration, these "arguments" have no types. We specify those in the instances below.
class First list x | list -> x

instance First Nil Nil

instance First (Cons x more) x

-- Note that these instances don't have the same "return type (kind?)."

-- ListConcat ------------------------------------------------------------------

class ListConcat a b c | a b -> c

instance ListConcat Nil x x

-- This one is crazy. The "cs" is recursed from the type constraint.
-- We iterate towards the base case by pulling things off the first argument.
instance (ListConcat as bs cs) => ListConcat (Cons a as) bs (Cons a cs)

-- ListConcatAll ---------------------------------------------------------------

class ListConcatAll ls l | ls -> l

instance ListConcatAll Nil Nil

-- This one is crazy, too, because two constraints are applied to accumulator.
-- How the fuck does that work?
-- Does one constraint occur before the other, or are they simultaneous?
instance
  ( ListConcat oneList accumulator result,
    ListConcatAll otherLists accumulator
  ) =>
  ListConcatAll (Cons oneList otherLists) result

-- AnyTrue ---------------------------------------------------------------------

class AnyTrue list t | list -> t

instance AnyTrue Nil False

instance AnyTrue (Cons True more) True

instance (AnyTrue list t) => AnyTrue (Cons False list) t

-- Booleans --------------------------------------------------------------------

data True

data False

-- Not -------------------------------------------------------------------------

class Not b1 b | b1 -> b

instance Not True False

instance Not False True

-- Or --------------------------------------------------------------------------

class Or b1 b2 b | b1 b2 -> b

instance Or False False False

instance Or True False True

instance Or False True True

instance Or True True True

-- Natural Numbers -------------------------------------------------------------

data Z

data S n

type N0 = Z

type N1 = S N0

type N2 = S N1

type N3 = S N2

type N4 = S N3

type N5 = S N4

type N6 = S N5

type N7 = S N6

type N8 = S N7

-- Equality --------------------------------------------------------------------

class PeanoEqual a b t | a b -> t

instance PeanoEqual Z Z True

instance PeanoEqual (S a) Z False

instance PeanoEqual Z (S b) False

instance (PeanoEqual a b t) => PeanoEqual (S a) (S b) t

-- Less Than -------------------------------------------------------------------

class PeanoLT a b t | a b -> t

instance PeanoLT Z Z False

instance PeanoLT (S a) Z False

instance PeanoLT Z (S b) True

instance (PeanoLT a b t) => PeanoLT (S a) (S b) t

-- Absolute Difference ---------------------------------------------------------

class PeanoAbsDiff a b c | a b -> c

instance PeanoAbsDiff Z Z Z

instance PeanoAbsDiff Z (S x) (S x)

instance PeanoAbsDiff (S x) Z (S x)

instance (PeanoAbsDiff a b c) => PeanoAbsDiff (S a) (S b) c

-- Range -----------------------------------------------------------------------

-- Oooh. I implemented this one without looking and I did it differently.
-- Mine is inclusive of "n", like range 3 = [0,1,2,3]
-- No, I just messed up the base case and was going to have two Zs at the end.

class Range n xs | n -> xs

instance Range Z Nil

instance (Range x lower) => Range (S x) (Cons x lower)

-- Comparison Check ------------------------------------------------------------

-- No idea what the fuck this is doing.
-- Well, that's not totally true. I think it's just giving us a way to access the type t through the legalCompare value.
class LegalCompare t | -> t where
  legalCompare :: t

-- One equals One
instance (PeanoEqual (S Z) (S Z) t) => LegalCompare t

-- But I don't think I can instance this for anything else, right?

-- My ghci doesn't show me anything useful when I do `:t legalCompare`, but if I try to evaluate legalCompare, it'll say "no instance for Show True."

class IllegalCompare t | -> t where
  illegalCompare :: t

instance (PeanoEqual True (Cons Z False) t) => IllegalCompare t

-- And the illegal compare looks like the t is ().

-- Apply -----------------------------------------------------------------------

data Conj1 list

class Apply f a r | f a -> r

-- There's nothing function-y about (Conj1 list).
-- We've just made an executive decision that when it's the first argument to Apply, the "output" type is a concatenation.
-- Structurally, there's nothing different between Apply and PeanoLT, we're just gonna think about Apply differently.
instance Apply (Conj1 list) x (Cons x list)

-- This really is defining what it means to apply type f to type a.
-- The similarity to function application is almost coincidental and shouldn't be thought into too much.
-- We just did it this way because it's familiar and is a good building block for the following steps.

-- Map -------------------------------------------------------------------------

class Map f xs ys | f xs -> ys

instance Map f Nil Nil

instance (Apply f x fx, Map f xs fxs) => Map f (Cons x xs) (Cons fx fxs)

-- MapCat ----------------------------------------------------------------------

class MapCat f xs zs | f xs -> zs

instance MapCat f Nil Nil

instance (Map f xs ys, ListConcatAll ys zs) => MapCat f xs zs

-- Append If -------------------------------------------------------------------

class AppendIf pred x ys zs | pred x ys -> zs

instance AppendIf False x ys ys

instance AppendIf True x ys (Cons x ys)

-- Filter ----------------------------------------------------------------------

class Filter fb xs ys | fb xs -> ys

instance Filter fb Nil Nil

-- Okay, here's a case where my implementation's constraints are in a different order.
-- Let's see if it has any impact.
instance
  (Apply fb x fx, AppendIf fx x accum output, Filter fb xs accum) =>
  Filter fb (Cons x xs) output

-- Queen -----------------------------------------------------------------------

data Queen x y

data QueenX x -- Incomplete queen that's used to help build from partial information

instance Apply (QueenX x) y (Queen x y)

-- Queens in a row -------------------------------------------------------------

class QueensInRow n x queens | n x -> queens

instance (Range n ys, Map (QueenX x) ys queens) => QueensInRow n x queens

-- Threatens -------------------------------------------------------------------

class Threatens q1 q2 bool | q1 q2 -> bool

instance
  ( PeanoEqual ax bx xeq,
    PeanoEqual ay by yeq,
    Or xeq yeq rookAttack,
    PeanoAbsDiff ax bx dx,
    PeanoAbsDiff ay by dy,
    PeanoEqual dx dy diagAttack,
    Or rookAttack diagAttack threatened
  ) =>
  Threatens (Queen ax ay) (Queen bx by) threatened

data Threatens1 q

instance (Threatens q q2 t) => Apply (Threatens1 q) q2 t

-- Safe ------------------------------------------------------------------------

class Safe otherQueens queen t | otherQueens queen -> t

instance
  ( Map (Threatens1 q) otherQueens listThreatened,
    AnyTrue listThreatened notSafe,
    Not notSafe sf
  ) =>
  Safe otherQueens q sf

data Safe1 otherQueens

instance (Safe otherQueens q sf) => Apply (Safe1 otherQueens) q sf

-- Add Queen -------------------------------------------------------------------

class AddQueen n x c cs | n x c -> cs

instance
  ( QueensInRow n x candidates, -- Generate n queens in row x
    Filter (Safe1 c) candidates filtered, -- Reduce candidates by existing placements
    Map (Conj1 c) filtered cs -- Return multiple configurations, each with a valid queen inserted
  ) =>
  AddQueen n x c cs

-- "Partially applied" AddQueen awaiting a config
data AddQueen2 n x

instance (AddQueen n x c cs) => Apply (AddQueen2 n x) c cs

-- Add Queen to all ------------------------------------------------------------

-- Setting up recursion over AddQueen since takes one config and returns multiple

class AddQueenToAll n x cs cs' | n x cs -> cs'

instance (MapCat (AddQueen2 n x) cs cs') => AddQueenToAll n x cs cs'

-- Recursion -------------------------------------------------------------------

class AddQueensIf pred n x cs cs' | pred n x cs -> cs'

instance AddQueensIf False n x cs cs

instance
  ( AddQueenToAll n x cs cs2,
    AddQueens n (S x) cs2 cs'
  ) =>
  AddQueensIf True n x cs cs'

class AddQueens n x cs cs' | n x cs -> cs'

instance (PeanoLT x n pred, AddQueensIf pred n x cs cs') => AddQueens n x cs cs'

-- Solution --------------------------------------------------------------------

class Solution n c | n -> c where
  solution :: n -> c

instance (AddQueens n Z (Cons Nil Nil) cs, First cs c) => Solution n c where
  solution = nil

-- To obtain solution in GHCi, run `:t solution (nil :: N6)`
