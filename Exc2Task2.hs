module Main where

import Text.Printf (printf)

main:: IO ()
main = do
  printf "%s\n" (if (isPrefixOf "pre" "prefix") then "True" else "False")
  printf "%s\n" (if (isPrefixOf "post" "prefix") then "True" else "False")

isPrefixOf :: String -> String -> Bool
isPrefixOf as bs
  | length as == 0 = True
  | length as > length bs = False
  | head as == head bs = isPrefixOf (tail as) (tail bs)
  | otherwise = False
