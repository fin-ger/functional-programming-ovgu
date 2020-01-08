module Main where

import Numeric.Natural (Natural)
import Text.Printf (printf)

main:: IO ()
main = do
  printf "%d 👉 %d\n" (quer 0) (0 :: Int)
  printf "%d 👉 %d\n" (quer 5) (5 :: Int)
  printf "%d 👉 %d\n" (quer 10) (1 :: Int)
  printf "%d 👉 %d\n" (quer 12) (3 :: Int)
  printf "%d 👉 %d\n" (quer 101) (2 :: Int)
  printf "%d 👉 %d\n" (quer 121) (4 :: Int)
  printf "%d 👉 borked\n" (quer$ -5)

quer :: Natural -> Natural
quer n = quer' n 0 where
  quer' :: Natural -> Natural -> Natural
  quer' n' r
    | n' == 0 = r
    | otherwise = quer' (n' `quot` 10) (n' `mod` 10 + r)
