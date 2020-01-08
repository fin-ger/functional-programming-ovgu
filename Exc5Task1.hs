module Main where

import Text.Printf (printf)
import Data.List (group, foldl1)

rle :: Eq a => [a] -> [(Int, a)]
rle = map (\i -> (length i, head i)) . group

lookAndSay :: [Int] -> [[Int]]
lookAndSay = iterate (concatMap (\(a, b) -> [a, b]) . rle)

say :: [[Int]] -> String
say (h:t) = (say' h) ++ " ~> " ++ (foldl1 (\a b -> a ++ ", " ++ b) . map say') t where
  say' = foldl (\a b -> a ++ (show b)) ""

main :: IO ()
main = do
  printf "%v\n" (show (rle [1, 2, 2, 3, 3, 3]))
  printf "%v\n" (show (rle [2, 2, 2, 1, 1, 3]))
  printf "%v\n" ((say.take 6.lookAndSay) [1, 2, 3])
  printf "%v\n" ((say.take 6.lookAndSay) [2, 2])
  printf "%v\n" ((say.take 6.lookAndSay) [4])
  printf "%v\n" ((say.take 6.lookAndSay) [9, 9])
  printf "%v\n" ((say.take 6.lookAndSay) [3, 2, 1])
