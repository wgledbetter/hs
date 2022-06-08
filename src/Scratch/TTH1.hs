{-# LANGUAGE TemplateHaskell #-}

-- Examples from https://www.youtube.com/watch?v=2uD6bCbL1-A
-- Benchmarking to quantify improvements

module Scratch.TTH1 where

-- import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- Binary Search Tree datatype -------------------------------------------------
data BST a
  = Node Int a (BST a) (BST a)
  | Leaf

-- Static tree instances -------------------------------------------------------
standardTable :: BST String
standardTable =
  Node
    5
    "root"
    ( Node
        3
        "left"
        (Node 2 "left-left" Leaf Leaf)
        (Node 4 "left-right" Leaf Leaf)
    )
    ( Node
        7
        "right"
        (Node 6 "right-left" Leaf Leaf)
        (Node 8 "right-right" Leaf Leaf)
    )

templatedTable :: (Quote m) => BST (Code m String)
templatedTable =
  Node
    5
    [||"root"||]
    ( Node
        3
        [||"left"||]
        (Node 2 [||"left-left"||] Leaf Leaf)
        (Node 4 [||"left-right"||] Leaf Leaf)
    )
    ( Node
        7
        [||"right"||]
        (Node 6 [||"right-left"||] Leaf Leaf)
        (Node 8 [||"right-right"||] Leaf Leaf)
    )

-- First pass runtime lookup function ------------------------------------------
standardLookup :: Int -> BST a -> Maybe a
standardLookup _ Leaf = Nothing
standardLookup i (Node j val l r) = case compare i j of
  LT -> standardLookup i l
  EQ -> Just val
  GT -> standardLookup i r

-- Hard-coded standard lookup with table ---------------------------------------
standardLookupTable :: Int -> Maybe String
standardLookupTable idx = standardLookup idx standardTable

-- TTH lookup function ---------------------------------------------------------
templatedLookup :: (Quote m) => Code m Int -> BST (Code m a) -> Code m (Maybe a)
templatedLookup _ Leaf = [||Nothing||]
templatedLookup i (Node j val l r) =
  [||
  case compare $$i $$(liftTyped j) of
    LT -> $$(templatedLookup i l)
    EQ -> Just $$val
    GT -> $$(templatedLookup i r)
  ||]

-- Hard-coded templated lookup with table --------------------------------------
