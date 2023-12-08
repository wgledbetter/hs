-- Signaling Adversity

module HB.Ch12 where

import Data.List (intercalate)
import HB.Ch10 (vowels)
import HB.Ch11 (BinaryTree (Leaf, Node), contains)

-- 12.2: Maybe -----------------------------------------------------------------

type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

-- "smart constructor"
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

-- 12.3: Either ----------------------------------------------------------------

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

-- Deriving Eq is not necessary for pattern matching, but _is_ necessary for comparisons (like equality in guards).

mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age
  | name == "" = Left NameEmpty
  | age < 0 = Left AgeTooLow
  | otherwise = Right $ Person name age

-- Functor won't map over left argument?

-- Combining error indications
type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> ValidatePerson Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> ValidatePerson Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

mkPerson'' :: Name -> Age -> ValidatePerson Person
mkPerson'' name age = mkPersonSub (nameOkay name) (ageOkay age)

mkPersonSub :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPersonSub (Right goodName) (Right goodAge) = Right (Person goodName goodAge)
mkPersonSub (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPersonSub (Left badName) _ = Left badName
mkPersonSub _ (Left badAge) = Left badAge

-- 12.4: Kinds -----------------------------------------------------------------
-- Higher-Kinded Types == type constructors
-- "lifted" types: *. Represented by pointers
-- "unlifted" types: #. Cannot be bottom. Native machine types.
-- newtype is unlifted because it is a thin wrapper around the inhabiting type.
-- "data constructors are functions"

-- 12.5: Exercises -------------------------------------------------------------
-- Determine the Kinds:
-- 1: *
-- 2: * and (* -> *)

-- String Processing
-- 1:
replaceThe :: String -> Maybe String
replaceThe [] = Nothing
replaceThe s = Just $ unwords [if isThe x then "a" else x | x <- words s]
  where
    isThe x = x == "the"

-- 2:
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = fromIntegral . length . (filter isTheVowel) . pairizeList . words
  where
    isTheVowel (f, s) = (f == "the") && (contains (head s) vowels)

pairizeList :: [a] -> [(a, a)]
pairizeList [] = []
pairizeList (x : []) = []
pairizeList (x : y : xs) = (x, y) : pairizeList (y : xs)

-- 3:
countVowels :: String -> Integer
countVowels = fromIntegral . length . (filter (\v -> contains v vowels))

-- Validate the word
newtype Word' = Word' String deriving (Eq, Show)

countConsonants :: String -> Integer
countConsonants = fromIntegral . length . (filter (\v -> not $ contains v vowels))

mkWord :: String -> Maybe Word'
mkWord w
  | countVowels w <= countConsonants w = Just $ Word' w
  | otherwise = Nothing

-- Natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat i = Succ $ integerToNat (i - 1)

-- Maybe Lib
-- 1:
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

-- 2:
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee fallback _ Nothing = fallback
mayybee _ f (Just x) = f x

-- 3:
fromMaybe :: a -> Maybe a -> a
fromMaybe fallback Nothing = fallback
fromMaybe _ (Just x) = x

-- 4:
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5:
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (m : ms) = case m of
  (Just x) -> x : catMaybes ms
  Nothing -> catMaybes ms

-- 6:
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (m : ms) = case m of
  Nothing -> Nothing
  (Just x) -> case flipMaybe ms of
    Nothing -> Nothing
    (Just xs) -> Just (x : xs)

-- Either Lib
-- 1:
lefts' :: [Either a b] -> [a]
lefts' =
  foldr
    ( \e l -> case e of
        Left x -> x : l
        _ -> l
    )
    []

-- 2:
rights' :: [Either a b] -> [b]
rights' =
  foldr
    ( \e l -> case e of
        Right x -> x : l
        _ -> l
    )
    []

-- 3:
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' =
  foldr
    ( \e (ls, rs) -> case e of
        Left x -> (x : ls, rs)
        Right x -> (ls, x : rs)
    )
    ([], [])

-- 4:
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f e = case e of
  (Left _) -> Nothing
  (Right x) -> Just (f x)

-- 5:
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl fr e = case e of
  (Left x) -> fl x
  (Right x) -> fr x

-- 6:
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\r -> Just (f r))

-- Unfolds
-- 1:
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

-- 2:
-- Type 'b' is the seed for the generation, and type 'a' is associated with the output.
-- The use of (b -> Maybe (a, b)) allows, at each recursion level, the calculation of this level's output and the seed value for the next level.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of
  Just (y, z) -> y : myUnfoldr f z
  Nothing -> []

-- 3:
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

-- Binary Tree
-- 1:
-- See notes on 'myUnfoldr'.
-- In this case, type 'a' is the seed, and type 'b' is associated with the output.
-- Generator function (a -> Maybe (a, b, a)) produces left seed, current node value, and right seed.
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
  Just (a, b, c) -> Node (unfold f a) b (unfold f c)
  Nothing -> Leaf

-- 2: wooo!
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x == n then Nothing else Just (x + 1, x, x + 1)) 0
