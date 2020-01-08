module Main where

import Text.Printf (printf)

product' :: [Int] -> Int
product' = foldl (\a b -> a * b) 1

noneIsEven :: [Int] -> Bool
noneIsEven = not.any even

sumLengths :: [String] -> Int
sumLengths = foldl (\a b -> a + (length b)) 0

xSquaredPlusThreeXPlusFive :: [(Integer, Integer)]
xSquaredPlusThreeXPlusFive = map (\x -> (x, x * x + 3 * x + 5)) [0 .. 150]

getByKey :: [(String, Int)] -> String -> [Int]
getByKey l k = (map (\(_,i) -> i) . filter (\(s,_) -> s == k)) l

main :: IO ()
main = do
  printf "product' [1 .. 5] ~> %i\n" (product' [1 .. 5])
  printf "noneIsEven [5, 7] ~> %v\n" (show (noneIsEven [5, 7]))
  printf "noneIsEven [4] ~> %v\n" (show (noneIsEven [4]))
  printf "sumLengths [\"Hallo\", \"DU!\"] ~> %i\n" (sumLengths ["Hallo", "DU!"])
  printf "xSquaredPlusThreeXPlusFive !! 1 ~> %v\n" (show (xSquaredPlusThreeXPlusFive !! 1))
  printf "getByKey [(\"a\", 2), (\"b\", 3), (\"a\", 6), (\"c\", 4)] ~> %v\n" (show (getByKey [("a", 2), ("b", 3), ("a", 6) , ("c", 4)] "a"))
