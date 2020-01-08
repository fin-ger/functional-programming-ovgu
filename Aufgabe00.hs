module Main where
fac :: Int -> Int
fac n = if n <= 0 then 1 else n * fac (n - 1)

main :: IO ()
main = print (fac 6)
