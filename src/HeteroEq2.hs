module HeteroEq2 () where

import Data.Dynamic

-- Standard higher-kinded type -------------------------------------------------

newtype MyEQ a = MyEQ a deriving (Eq)

-- The function I need ---------------------------------------------------------

myEq :: (Eq a, Typeable a, Typeable b) => a -> b -> Bool
myEq x y = case fromDynamic $ toDyn y of
  Just b -> x == b
  Nothing -> False
