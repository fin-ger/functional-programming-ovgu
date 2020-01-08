module Main where

(💩) :: Bool
(💩) = True

sum :: Int -> Int
sum n = sum' n 1 where
  sum' n r
    | n == 1 = r
    | n == 0 = error "redirecting to /dev/null..."
    | n < 1 = error "you can't be negative in the freedom dimension"
    | (💩) = sum' (n - 1) (r + n)

main :: IO ()
main = do
  print (Main.sum 5)
  print (Main.sum (-3))
