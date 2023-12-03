-- Algebraic Datatypes
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HB.Ch11 where

import Data.Char
import Data.Int
import HB.Ch09 (charShift, squish)

-- 11.2: Data Declarations Review ----------------------------------------------

-- Type constructor vs. Data constructor
-- Type constructor can take a type as an argument (e.g. list of _what_?)
-- Data constructor takes values at "term-level."

-- 11.3: Data and type constructors --------------------------------------------
-- Constants vs. Constructors. constants are constructors with no arguments.

-- 11.4: Type constructors and kinds -------------------------------------------
-- A * is a kind. A type constructor that is not yet a kind will be something like (* -> *).
-- For example, the kind of [] is (* -> *) because you don't know what it's a list _of_ yet.

-- 11.5: Data constructors and values ------------------------------------------
-- Phantom type argument example:
data HuskyType a = HuskyData

-- The type constructor needs a type, but it is unused in any data constructors.

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

-- 11.5 Exercises
-- 1: Type constructor
-- 2: * -> *
-- 3: *
-- 4: (Num a) => Doggies a
-- 5: Doggies Integer
-- 6: Doggies String
-- 7: Yes... Both. Depends on the context.
-- 8: doge -> DogueDeBordeaux doge
-- 9: DogueDeBordeaux String

-- 11.6 ------------------------------------------------------------------------

data Price = Price Integer deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Size
  = EensyWeensy
  | Smol
  | Yuge
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir Smol

-- Exercises:
-- 1: Vehicle
-- 2:
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

-- 3:
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

-- 4: bottom
-- 5: Done

-- 11.7: Data constructor arities ----------------------------------------------
-- Arity = number of arguments
-- - nullary = 0 (e.g. nullary constructor, nullary function)
data Test = Fake Int String deriving (Show)

ff = Fake 23

-- Tuples are "anonymous products"

-- 11.8: Why algebraic? --------------------------------------------------------
-- Because of the "sum" and "product."
-- How many possible values of this type are there?
--   Suppose `data Test = Test T1 T2`.
--   The number of valid `Test`s would be the number of valid `T1`s _times_ the number of valid `T2`s.
--   Similarly, in the case of `data Test = This T1 | That T2`, the number of valid `Test`s is n_T1 _plus_ n_T2.
--   Or, more close to home, `data Vec2 = Vec2 Double Double` _is_ R^2.

-- Exercises: Cardinality
-- 1: 1
-- 2: 3
-- 3: 65536
-- 4: Int is big (18446744073709551616), Integer is unbounded (literally has no instance of Bounded)
-- 5: 2^8

-- Exercises: Example
data Example = MakeExample deriving (Show)

-- 1: MakeExample :: Example
-- 2: The kind of Example is *. The Show instance is defined on line 119.
-- 3: The constructor now has function syntax, the type's kind (?) is still *, same typeclass instances.
data Example2 = MakeAnotherExample Int8 deriving (Show)

-- With respect to cardinality, unary constructors are just the identity function.

-- 11.9: newtype ---------------------------------------------------------------
-- Only permits unary constructors. No nullary, no sum, no product.
-- Allows for compiler reduction to the contained type. Zero runtime overhead.

newtype Goats = Goats Int deriving (Eq, Show)

newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 45

instance TooMany Cows where
  tooMany (Cows n) = n > 40

-- Even though the newtypes Goat and Cow "are" Ints, we can use their type to trigger different behavior (and enforce safety).
-- This is not possible via simple type aliases.

-- Language extension `GeneralizedNewtypeDeriving` would allow us to derive the Int instance of TooMany for Goats and Cows.
newtype Chickens = Chickens Int deriving (Eq, Show, TooMany)

-- Exercises: newtype
-- 1:
newtype IntString = IntString (Int, String) deriving (Eq, Show)

instance TooMany IntString where
  tooMany (IntString (i, s)) = i > length s

-- 2:
newtype IntInt = IntInt (Int, Int) deriving (Eq, Show)

instance TooMany IntInt where
  tooMany (IntInt (a, b)) = (a + b) > 42

-- 3:
newtype ManyTuple a = ManyTuple (a, a) deriving (Eq, Show)

instance (Num a, TooMany a) => TooMany (ManyTuple a) where
  tooMany (ManyTuple (x, y)) = (tooMany x) || (tooMany y)

-- 11.10: Sum Types ------------------------------------------------------------

-- 11.10 Exercises
-- 1: 4 (2+2)
-- 2:
data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

myNumba = Numba (-128)

-- Cardinality = 258 = 2+256
-- Overflow/wraparound

-- 11.11: Product Types --------------------------------------------------------
data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)

data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show) -- Card = 9

-- Record Syntax
data Person = Person {name :: String, age :: Int} deriving (Eq, Show)

-- In the above, `name` and `age` are actually functions that get the String and Int from a Person type.
-- Thus, I cannot simultaneously define:
-- data Pet = Pet {name :: String, age :: Int} deriving (Eq, Show)
-- because the `name` and `age` identifiers are already taken in this module.

stupidSomething :: Person -> Char
stupidSomething (Person s i) = s !! i

-- Looks like you _can_ still pattern match based on the order the records were defined.

data Quaternion a = Quaternion
  { qr :: a, -- Real Component
    qi :: a,
    qj :: a,
    qk :: a
  }
  deriving (Eq, Show)

-- This doesn't make it _impossible_ to accidently assume the scalar is the last entry, but it does help.

-- 11.12: Normal Form ----------------------------------------------------------

data Fiction = FictionCtor deriving (Show)

data Nonfiction = NonfictionCtor deriving (Show)

data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving (Show)

type AuthorName = String -- Alias

data AuthorNon = AuthorNon (AuthorName, BookType) -- Not normal form. Expansion hidden beneath BookType

data AuthorNrm = Fiction AuthorName | Nonfiction AuthorName -- Normal Form. Note that these `Fiction` and `Nonfiction` constructors have no relation to the ones defined above. Neither the types nor the constructors.

-- Exercises
data FlowerType
  = GardeniaF
  | DaisyF
  | RoseF
  | LilacF
  deriving (Show)

type Gardener = String

data GardenNNF = GardenNNF Gardener FlowerType deriving (Show)

-- 1:
data GardenNF
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving (Show)

-- 11.13: Constructing and Destructing Values ----------------------------------

data GuessWhat = ChickenButt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pfirst :: a,
    psecond :: b
  }
  deriving (Eq, Show)

-- Exercises
data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show, Enum, Ord, Bounded)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show, Enum, Ord, Bounded)

data Programmer = Programmer
  { os :: OperatingSystem,
    lang :: ProgLang
  }
  deriving (Eq, Show)

allProgs :: [Programmer]
allProgs =
  [ Programmer {os = o, lang = l}
    | o <- enumFrom (minBound :: OperatingSystem),
      l <- enumFrom (minBound :: ProgLang)
  ]

-- I solved this by adding Enum, Ord, and Bounded to the deriving clause of OperatingSystem and ProgLang.
-- I don't necessarily think that's a great solution, because what does it _mean_ for Haskell to be the lower bound of programming languages?
-- Nothing. It just saves me some effort because I don't have to re-write my constructors into a list.
-- I'd still write the solution the same way with set notation or whatever.

-- Accidental bottoms from records
partialProg = Programmer {os = GnuPlusLinux} -- This is hiding an exception inside it

okay = os partialProg

partialProg2 = Programmer {lang = Haskell} -- Same latent bottom as above

okay2 = lang partialProg2

partialProg' = Programmer GnuPlusLinux -- This is just partially applied.

-- Deconstructing Values

-- c Null will hit bottom
data Bad
  = NullBad
  | Something
      { aBad :: Int,
        bBad :: Double,
        cBad :: String
      }
  deriving (Show)

data Internal = InternalThing
  { a :: Int,
    b :: Double,
    c :: String
  }
  deriving (Show)

data Good = NullGood | Thing Internal deriving (Show)

-- 11.14: Function Type is Exponential -----------------------------------------
-- a -> b has b^a inhabitants, e.g. there are b^a ways to implement a function a -> b.
-- a -> b -> c = (c^b)^a

conv1 :: QuantumBool -> Bool
conv1 QuantumTrue = True
conv1 QuantumFalse = True
conv1 QuantumBoth = True

conv2 :: QuantumBool -> Bool
conv2 QuantumTrue = True
conv2 QuantumFalse = True
conv2 QuantumBoth = False

conv3 :: QuantumBool -> Bool
conv3 QuantumTrue = True
conv3 QuantumFalse = False
conv3 QuantumBoth = True

conv4 :: QuantumBool -> Bool
conv4 QuantumTrue = True
conv4 QuantumFalse = False
conv4 QuantumBoth = False

conv5 :: QuantumBool -> Bool
conv5 QuantumTrue = False
conv5 QuantumFalse = True
conv5 QuantumBoth = True

conv6 :: QuantumBool -> Bool
conv6 QuantumTrue = False
conv6 QuantumFalse = True
conv6 QuantumBoth = False

conv7 :: QuantumBool -> Bool
conv7 QuantumTrue = False
conv7 QuantumFalse = False
conv7 QuantumBoth = True

conv8 :: QuantumBool -> Bool
conv8 QuantumTrue = False
conv8 QuantumFalse = False
conv8 QuantumBoth = False

-- Exercise: The Quad
-- 1: 8 (4+4)
-- 2: 16 (4*4)
-- 3: 256 (4^4)
-- 4: 8 (2*2*2)
-- 5: 16 ((2^2)^2)
-- 6: 65536 ((2^4)^4)

-- 11.15: Higher-Kinded Datatypes ----------------------------------------------
-- Any un-applied type arguments: [a], (a,b), etc.
-- If the kind has a "->", it's "higher kind"

-- 11.16: Lists are Polymorphic ------------------------------------------------
-- All infix type/data constructors must start with a colon.

-- 11.17: Binary Tree ----------------------------------------------------------
data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a) -- Left, Center, Right
  deriving (Eq, Ord, Show)

insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert' val Leaf = Node Leaf val Leaf
insert' val (Node tl tv tr)
  | val == tv = Node tl tv tr
  | val < tv = Node (insert' val tl) tv tr
  | val > tv = Node tl tv (insert' val tr)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node tl val tr) = Node (mapTree f tl) (f val) (mapTree f tr)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node tl val tr) = [val] ++ preorder tl ++ preorder tr

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node tl val tr) = inorder tl ++ [val] ++ inorder tr

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node tl val tr) = postorder tl ++ postorder tr ++ [val]

-- Just one interpretation. Free to swap foldr for foldl or preorder to any other list ordering.
-- The problem with doing a tree fold without a list mapping is how do you combine the left and right sides of the tree?
-- I'm brainstorming something like "foldTree f v (Node tl val tr) = f (foldTree f v tl) val" but then where does "tr" go?
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f v t = foldr f v (preorder t)

-- 11.18: Exercises ------------------------------------------------------------
-- Multiple Choice:
-- 1: A
-- 2: C
-- 3: B
-- 4: C

-- Ciphers

repeatUntilLength :: Int -> [b] -> [b]
repeatUntilLength targLen l = take targLen (squish $ repeat l)

-- Assumes the [Int] is sorted low to high
insertAt :: [Int] -> a -> [a] -> [a]
insertAt [] _ list = list
insertAt (firstPlace : otherPlaces) item list =
  listFront
    ++ [item]
    ++ insertAt (map (\op -> op - firstPlace - 1) otherPlaces) item listBack
  where
    listFront = take firstPlace list
    listBack = drop firstPlace list

idxsOf :: (Eq a) => a -> [a] -> [Int]
idxsOf _ [] = []
idxsOf c l = go 0 c l
  where
    go _ _ [] = []
    go idx item (x : xs)
      | item == x = idx : go (idx + 1) item xs
      | otherwise = go (idx + 1) item xs

vigenereEncode :: String -> String -> String
vigenereEncode key msg = map (uncurry charShift) (zip shifts capMsg) -- apply shift to each msg char
  where
    capKey = map (toUpper) key -- change to caps
    capMsg = map (toUpper) msg -- change to caps
    longKey = repeatUntilLength (length msg) capKey -- repeat key to fit message length
    longKeySpaces = insertAt (idxsOf ' ' msg) 'A' longKey -- skip message spaces
    shifts = map (\c -> ord c - (ord 'A')) longKeySpaces -- find shift for each character

vigenereDecode :: String -> String -> String
vigenereDecode key msg = map (uncurry charShift) (zip shifts capMsg)
  where
    capKey = map (toUpper) key
    capMsg = map (toUpper) msg
    longKey = repeatUntilLength (length msg) capKey
    longKeySpaces = insertAt (idxsOf ' ' msg) 'A' longKey
    shifts = map (\c -> ord 'A' - (ord c)) longKeySpaces -- only diff btw encode and decode

-- As-patterns
-- 1: sub-sequences
contains :: (Eq a) => a -> [a] -> Bool
contains _ [] = False
contains i (x : xs)
  | i == x = True
  | otherwise = contains i xs

-- This doesn't use an as-pattern...
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf _ [] = False
isSubseqOf [] _ = True
isSubseqOf (s1 : ss) xs = isSubseqOf ss (drop idx xs)
  where
    has = contains s1 xs
    idx = if has then idxsOf s1 xs !! 0 else length xs -- if not has, drop all xs

-- 2: capitalization
capAndNot :: String -> (String, String)
capAndNot w@(f : r) = (w, toUpper f : r)

capitalizeWords :: String -> [(String, String)]
capitalizeWords = (map capAndNot) . words

-- Language exercises
-- 1:
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (f : r) = toUpper f : r

-- 2:
splitAndDropIdxs :: [Int] -> [a] -> [[a]]
splitAndDropIdxs _ [] = []
splitAndDropIdxs [] l = [l]
splitAndDropIdxs (i : is) l = take i l : splitAndDropIdxs (map (\x -> x - i - 1) is) (drop (i + 1) l)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn x l = splitAndDropIdxs (idxsOf x l) l

squishWith :: a -> [[a]] -> [a]
squishWith _ [] = []
squishWith y (x : xs) = x ++ (y : squishWith y xs)

capitalizeParagraph :: String -> String
capitalizeParagraph =
  (squishWith ' ')
    . squish
    . (map (\l -> init l ++ [last l ++ "."]))
    . (map (\(x : xs) -> capitalizeWord x : xs))
    . (map words)
    . (splitOn '.')

-- Phone exercise
-- 1:
data DaPhone = DaPhone
  { keys :: [Char], -- Key labels
    perKey :: [Int], -- num characters per key
    chars :: [Char] -- characters
  }

ph =
  DaPhone
    { keys = "123456789*0#",
      perKey = [0, 3, 3, 3, 3, 3, 4, 3, 4, 1, 2, 2],
      chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ^+ .,"
    }

tap :: DaPhone -> Char -> Int -> Char
tap ph c n = chars ph !! (charBaseIdx + (mod (n - 1) perThisKey))
  where
    keyIdx = idxsOf c (keys ph) !! 0
    charBaseIdx = foldr (+) 0 (take keyIdx (perKey ph))
    perThisKey = perKey ph !! keyIdx

-- 2:
reverseTaps :: DaPhone -> Char -> (Char, Int)
reverseTaps ph c = (keys ph !! keyIdx, charIdx - (charBaseIdxs !! (keyIdx - 1)) + 1)
  where
    charIdx = idxsOf c (chars ph) !! 0
    charBaseIdxs = tail $ scanl (+) 0 (perKey ph)
    keyIdx = length $ filter (<= charIdx) charBaseIdxs

cellPhonesDead :: DaPhone -> String -> [(Char, Int)]
cellPhonesDead ph = map (reverseTaps ph)

-- 3:
fingerTaps :: [(Char, Int)] -> Int
fingerTaps = foldr (\(_, t) acc -> acc + t) 0

-- 4:

-- NOT FINISHING THE PHONE EXERCISE BECAUSE I WANT TO MOVE ON.
-- I'LL ACCEPT THIS AS STRIKE 1.

-- Hutton's Razor
-- 1:
data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)
