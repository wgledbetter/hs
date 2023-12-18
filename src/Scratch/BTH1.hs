{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- Implementing and measuring examples from https://www.youtube.com/watch?v=AzJVFkm42zM

module Scratch.BTH1 where

import Language.Haskell.TH (Code, Quote)

-- Power Example ---------------------------------------------------------------

power5 :: Int -> Int
power5 k = k * k * k * k * k

power :: Int -> Int -> Int
power 0 k = 1
power n k = k * power (n - 1) k

power5' :: Int -> Int
power5' = power 5

tPower :: (Quote m) => Int -> Code m (Int -> Int)
tPower 0 = [||\k -> 1||]
tPower n = [||\k -> k * $$(tPower (n - 1)) k||]

-- -- SQL Example -----------------------------------------------------------------

-- -- Without Templates

-- type Table = [String -> [Int]]

-- type Schema = [String]

-- data Record = Record
--   { schema :: [ByteString],
--     fields :: [ByteString]
--   }

-- type Predicate = Record -> Bool

-- data Operator
--   = Scan Table
--   | Project Schema Schema Operator
--   | Filter Predicate Operator
--   | Join Operator Operator

-- execOp :: Operator -> IO [Record]
-- execOp (Project newSchema parentSchema o) = do
--   rs <- execOp o
--   return (map (restrict newSchema parentSchema) rs)

-- restrict :: Schema -> Schema -> Record -> Record
-- restrict = _

-- -- With Templates

-- data Record' = Record'
--   { schema' :: [ByteString],
--     fields' :: [Code (* -> *) ByteString]
--   }

-- data Operator'
--   = Scan' Table
--   | Project' Schema Schema Operator'
--   | Filter' Predicate Operator'
--   | Join' Operator' Operator'

-- execOp' :: (Monoid m, Quote q) => Operator' -> (Record' -> Code q (IO m)) -> Code q (IO m)
-- execOp' op yld =
--   case op of
--     Scan' file sch -> processCSV sch file yld
--     Filter' predicate parent ->
--       execOp'
--         parent
--         ( \rcrd ->
--             [||whenM $$(evalPred predicate rcrd) $$(yld rcrd)||]
--         )
--     Project' newSchema parentSchema parent ->
--       execOp'
--         parent
--         ( \rcrd -> yld (restrict' rcrd newSchema parentSchema)
--         )
--     Join' left right -> fake

-- processCSV' :: (forall m. Monoid m, Quote q) => Schema -> FilePath -> (Record' -> Code q (IO m)) -> Code q (IO m)
-- processCSV' ss f yld =
--   [||
--   do
--     bs <- newScanner f
--     $$(rows ss) bs
--   ||]
--   where
--     rows :: Schema -> Code q (Scanner -> IO m)
--     rows sch = do
--       while
--         [||hasNext||]
--         [||
--         \r rs -> do
--           $$( let (hs, ts) = nextLine [||rs||]
--                in [||
--                   $$(yld (Record' (parseRow sch hs) sch))
--                     <> r $$(ts)
--                   ||]
--             )
--         ||]

-- queryProj :: Operator'
