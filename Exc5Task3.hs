module Main where

import Text.Printf (printf)
import Data.List (scanl, sortOn)

partialSums :: Num a => [a] -> [a]
partialSums = scanl (+) 0

toSameLength :: [String] -> [String]
toSameLength l = let max = (maximum . map length) l in
  map (\s -> s ++ (replicate (max - length s) ' ')) l

sortByLength :: [[a]] -> [[a]]
sortByLength = sortOn length

sortByLengthRememberingIndex :: [[a]] -> [(Int, [a])]
sortByLengthRememberingIndex = sortOn (length . snd) . zip [0 ..]

innerOuterMap :: (a -> b) -> ([b] -> c) -> [[a]] -> [c]
innerOuterMap f1 f2 = map (f2 . map f1)

main :: IO ()
main = do
  printf "partialSums [1, 2, 3, 4] ~> %v\n" (show (partialSums [1, 2, 3, 4]))
  printf "toSameLength [\"Christoph\", \"Lueth\"] ~> %v\n" (show (toSameLength ["Christoph", "Lueth"]))
  printf "sortByLength [[3, 4, 5], [11], [1]] ~> %v\n" (show (sortByLength [[3, 4, 5], [11], [1]]))
  printf "sortByLengthRemembering [\"abc\", \"p\", \"xx\"] ~> %v\n" (show (sortByLengthRememberingIndex ["abc", "p", "xx"]))
  printf "innerOuterMap (*3) sum [[1..4], [5, 6]] ~> %v\n" (show (innerOuterMap (*3) sum [[1..4], [5, 6]]))
