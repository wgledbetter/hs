-- Typeclasses -----------------------------------------------------------------
instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "Just " ++ show x

-- Show, Read, Eq, Ord, Bounded, Enum
-- "deriving"

-- fmap :: (a -> b) -> c a -> c b
-- ap :: c (a -> b) -> c a -> c b
-- bind :: (a -> c b) -> c a -> c b

-- fmap: apply generic functions on contextualized types
fmap show [1, 2, 3] = ["1", "2", "3"]

show <$> (Just 42) = Just "42"

-- Example: sum of two Maybe Int
fmap (+) (Just 3) = Just (3 +)
ap (Just (3+)) (Just 39) = Just 42
Just (3+) <*> Just 39 = Just 42
-- f {a -> b -> c} <$> Context a <*> Context b = Context c {Effectively, f a b}

-- >>= is bind
div2 :: Int -> Maybe Int
(>>=) :: c a -> (a -> c b) -> c b
div16 :: Int -> Maybe Int
div16 x = div2 x >>= div2 >>= div2 >>= div2

-- fmap: Functor
-- ap: Applicative
-- bind: Monad

-- IO --------------------------------------------------------------------------
main = getFirstName >>= \firstname -> getLastName >>= \lastname -> greetUser firstname lastname

main = do
  firstname <- getFirstName
  lastname <- getLastName
  greetUser firstname lastname

-- Codelab Notes ---------------------------------------------------------------

addColorToMap :: Color -> ColorMap -> ColorMap
addColorToMap color cmap = let currentCount = getCount color cmap in insert color (currentCount + 1) cmap
addColorToMap color cmap = insertWith (+) color 1 cmap

-- Object modifiers typicall end with the object to make for easier chaining via currying
