module Main where

import Text.Printf (printf)
import Data.List (foldl, foldr)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy comp (item:list) = foldl groupBy' [[item]] list where
  groupBy' list b = if comp (head (last list)) b
    then (init list) ++ [(last list) ++ [b]]
    else list ++ [[b]]

groupby :: Eq b => [a] -> (a -> b) -> [(b, [a])]
groupby list key =
  (
    map (\i -> (fst (head i), map snd i)) .
    groupBy (\a b -> (fst a) == (fst b)) .
    map (\i -> (key i, i))
  ) list

main :: IO ()
main = do
  printf "%s\n" (show (groupBy (<=) [1 :: Int,2,2,3,1,2,0,4,5,2]))
  printf "%s\n" (show (groupby ["one", "two", "three", "four",
                                "five", "six", "seven", "eight",
                                "nine", "ten"] length))
