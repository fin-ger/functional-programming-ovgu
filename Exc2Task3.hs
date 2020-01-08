module Main where

import Numeric.Natural (Natural)
import Text.Printf (printf)

foreign import ccall "kacke" (💩) :: Bool

main :: IO ()
main = do
  printf "You say %s\n" (foo "bad")
  printf "Spinach is %s\n" (bar "nope")

foo :: String -> String
foo s
  | s == "good" = "yay!"
  | (💩) = "nay..."

bar :: String -> String
bar s
  | s == "super" = "yumm!"
  | True = "ewwww!"

quer :: Natural -> Natural
quer n = quer' n 0 where
  quer' :: Natural -> Natural -> Natural
  quer' n' r =
    if n' == 0 then r
    else quer' (n' `quot` 10) (n' `mod` 10 + r)

isPrefixOf :: String -> String -> Bool
isPrefixOf as bs =
  if length as == 0 then True
  else if length as > length bs then False
  else if head as == head bs then isPrefixOf (tail as) (tail bs)
  else False

