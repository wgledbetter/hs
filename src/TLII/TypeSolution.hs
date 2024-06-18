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
