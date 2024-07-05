{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TLII.TypeSolution where

import Prelude (undefined)

-- Colors ----------------------------------------------------------------------

-- By omitting constructors, `undefined` is the only valid value.
-- These are "atomic constants," meaning they cannot be subdivided into smaller parts.

data R

data G

data B

data W

-- Cube (and parameterized types) ----------------------------------------------

data Cube u f r b l d

-- Similarly to the colors, this has no constructors.

type Cube1 = Cube B G W G B R

type Cube2 = Cube W G B W R R

type Cube3 = Cube G W R B R R

type Cube4 = Cube B R G G W W

-- Transforms ------------------------------------------------------------------

-- Nothing inside these functions matter. We can define their entire purpose with the types.

class Transforms u f r b l d where
  rot :: Cube u f r b l d -> Cube u r b l f d
  twist :: Cube u f r b l d -> Cube f r u l d b
  flip :: Cube u f r b l d -> Cube d l b r f u

-- We can also make a generic instance that's valid for any type arguments:
instance Transforms u f r b l d where
  rot = undefined
  twist = undefined
  flip = undefined

-- You could have also defined these functions outside a typeclass, I'm pretty sure.A

-- Functional Dependencies -----------------------------------------------------

data True

data False

-- Ah! This pipe is a "functional dependency" of type b on types b1 and b2.
-- The TypeInterview example completely omitted the functions of the class, too.
-- Wonder if that'll turn out to be significant. It might just end up as a more familiar way to tie it all together into a solution definition at the end.
class And b1 b2 b | b1 b2 -> b where
  and :: b1 -> b2 -> b

-- These instance definitions are the same as before.

instance And True True True where and = undefined

instance And True False False where and = undefined

instance And False True False where and = undefined

instance And False False False where and = undefined

-- Lists -----------------------------------------------------------------------

data Nil

data Cons x xs

-- Type-level infix operators!!!
-- Must begin with :

data x ::: xs

infixr 5 :::

-- Type Constraints ------------------------------------------------------------

class ListConcat l1 l2 l | l1 l2 -> l where
  listConcat :: l1 -> l2 -> l

instance ListConcat Nil l l where listConcat = undefined

instance (ListConcat xs ys zs) => ListConcat (x ::: xs) ys (x ::: zs) where
  listConcat = undefined

-- Apply -----------------------------------------------------------------------

-- Ah, defining the typeclass functions prevents them from being used in this "apply" pattern.

class Apply f a b | f a -> b where
  apply :: f -> a -> b

data Rotate

data Twist

data Flip

instance Apply Rotate (Cube u f r b l d) (Cube u r b l f d) where apply = undefined

instance Apply Twist (Cube u f r b l d) (Cube f r u l d b) where apply = undefined

instance Apply Flip (Cube u f r b l d) (Cube d l b r f u) where apply = undefined

-- Oh, but also, the functions inside the typeclasses provide a better interface to query the "output" type

-- Map -------------------------------------------------------------------------

class Map f xs zs | f xs -> zs where
  map :: f -> xs -> zs

instance Map f Nil Nil where map = undefined

instance (Apply f x y, Map f xs ys) => Map f (x ::: xs) (y ::: ys) where map = undefined

-- Filter ----------------------------------------------------------------------

class Filter f xs zs | f xs -> zs where
  filter :: f -> xs -> zs

instance Filter f Nil Nil where filter = undefined

instance
  ( Apply f x b,
    Filter f xs ys,
    PrependIf b x ys zs
  ) =>
  Filter f (x ::: xs) zs
  where
  filter = undefined

class PrependIf b x ys zs | b x ys -> zs

instance PrependIf True x ys (x ::: ys)

instance PrependIf False x ys ys

-- List Comprehensions ---------------------------------------------------------

-- Analogous to (xs ++ map f xs)
class MapAppend f xs zs | f xs -> zs where
  mapAppend :: f -> xs -> zs

instance MapAppend f Nil Nil where
  mapAppend = undefined

instance (Map f xs ys, ListConcat xs ys zs) => MapAppend f xs zs where
  mapAppend = undefined

-- Analogous to (xs ++ (map f xs) ++ (map f (map f xs)))
class MapAppend2 f xs zs | f xs -> zs where
  mapAppend2 :: f -> xs -> zs

instance MapAppend2 f Nil Nil where
  mapAppend2 = undefined

instance (Map f xs ys, MapAppend f ys ys', ListConcat xs ys' zs) => MapAppend2 f xs zs where
  mapAppend2 = undefined

-- Analogous to (xs ++ (map f xs) ++ (map f (map f xs)) ++ (map f (map f (map f xs))))
class MapAppend3 f xs zs | f xs -> zs where
  mapAppend3 :: f -> xs -> zs

instance MapAppend3 f Nil Nil where
  mapAppend3 = undefined

instance (Map f xs ys, MapAppend2 f ys ys', ListConcat xs ys' zs) => MapAppend3 f xs zs where
  mapAppend3 = undefined

-- Orientations ----------------------------------------------------------------

-- A "function" that generates all orientations of a given cube.
-- Since it's acting like a function, we have to specify how to Apply it.
data Orientations

instance
  ( MapAppend Flip (c ::: Nil) fs,
    MapAppend2 Twist fs ts,
    MapAppend3 Rotate ts zs
  ) =>
  Apply Orientations c zs
  where
  apply = undefined

-- Not Equal -------------------------------------------------------------------

-- Used to compare whether the same face is visible
class NEQ x y b | x y -> b where
  neq :: x -> y -> b

instance NEQ R R False where neq = undefined

instance NEQ R G True where neq = undefined

instance NEQ R B True where neq = undefined

instance NEQ R W True where neq = undefined

instance NEQ G R True where neq = undefined

instance NEQ G G False where neq = undefined

instance NEQ G B True where neq = undefined

instance NEQ G W True where neq = undefined

instance NEQ B R True where neq = undefined

instance NEQ B G True where neq = undefined

instance NEQ B B False where neq = undefined

instance NEQ B W True where neq = undefined

instance NEQ W R True where neq = undefined

instance NEQ W G True where neq = undefined

instance NEQ W B True where neq = undefined

instance NEQ W W False where neq = undefined

-- All -------------------------------------------------------------------------

-- Really more like the "and" function
class All l b | l -> b where
  all :: l -> b

instance All Nil True where
  all = undefined

instance All (False ::: xs) False where
  all = undefined

instance (All xs b) => All (True ::: xs) b where
  all = undefined

-- Cube Compatibility ----------------------------------------------------------

class Compatible c1 c2 b | c1 c2 -> b where
  compatible :: c1 -> c2 -> b

-- Pick out the visible faces and check equality over all
instance
  ( NEQ f1 f2 bF,
    NEQ r1 r2 bR,
    NEQ b1 b2 bB,
    NEQ l1 l2 bL,
    All (bF ::: bR ::: bB ::: bL ::: Nil) b
  ) =>
  Compatible (Cube u1 f1 r1 b1 l1 d1) (Cube u2 f2 r2 b2 l2 d2) b
  where
  compatible = undefined

-- Allowed ---------------------------------------------------------------------

class Allowed c cs b | c cs -> b where
  allowed :: c -> cs -> b

instance Allowed c Nil True where
  allowed = undefined

instance
  ( Compatible c y bC,
    Allowed c ys bA,
    And bC bA b
  ) =>
  Allowed c (y ::: ys) b
  where
  allowed = undefined

-- Solution --------------------------------------------------------------------

-- The last-minute introduction of AllowedCombinations and MatchingOrientations
--   is very "draw the rest of the owl."

class Solutions cs ss | cs -> ss where
  solutions :: cs -> ss

instance Solutions Nil (Nil ::: Nil) where
  solutions = undefined

instance
  ( Solutions cs sols,
    Apply Orientations c os,
    AllowedCombinations os sols zs
  ) =>
  Solutions (c ::: cs) zs
  where
  solutions = undefined

class AllowedCombinations os sols ss | os sols -> ss

instance AllowedCombinations os Nil Nil

instance
  ( AllowedCombinations os sols as,
    MatchingOrientations os s bs,
    ListConcat as bs ss
  ) =>
  AllowedCombinations os (s ::: sols) ss

class MatchingOrientations os sol zs | os sol -> zs

instance MatchingOrientations Nil sol Nil

instance
  ( MatchingOrientations os sol as,
    Allowed o sol b,
    PrependIf b (o ::: sol) as zs
  ) =>
  MatchingOrientations (o ::: os) sol zs

type Cubes = (Cube1 ::: Cube2 ::: Cube3 ::: Cube4 ::: Nil)

ans = solutions (undefined :: Cubes)
