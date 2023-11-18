module HB.Ch03 where

main1 :: IO ()
main1 = putStrLn "Hello there."

main2 :: IO ()
main2 = do
  putStrLn "wow"
  putStr "fake and "
  putStr "gay"
  putStrLn " is not a politically correct statement anymore."

concat1 :: String
concat1 = "wow" ++ " such doge"

concat2 :: String
concat2 = concat ["im", " ", "bored"]

typedWhere :: Int -> Int
typedWhere x =
  x + theValue
  where
    theValue :: Int
    theValue = 48

-- Chapter Exercises -----------------------------------------------------------
func1 :: String -> String
func1 = tail

funcA :: String -> String
funcA s = s ++ "!"

funcB :: String -> Char
funcB = (!! 4)

funcC :: String -> String
funcC = drop 9

func3 :: String -> Char
func3 = (!! 2)

func4 :: Int -> Char
func4 = (!!) "Curry is awesome!"

func5 :: String -> String
func5 s = concat [drop 9 s, " ", take 2 $ drop 6 s, " ", take 5 s]
