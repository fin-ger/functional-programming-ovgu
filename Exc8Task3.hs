module Main where

import Control.Monad (ap, liftM)

data Identity a = Identity a
  deriving Show
instance Functor Identity where
  fmap = liftM
instance Applicative Identity where
  pure = return
  (<*>) = ap
instance Monad Identity where
  (Identity item) >>= func = func item
  return item = Identity item

data CountBinds a = CountBinds (Integer, a)
  deriving Show
binds :: CountBinds a -> Integer
binds (CountBinds (count, _)) = count
instance Functor CountBinds where
  fmap = liftM
instance Applicative CountBinds where
  pure = return
  (<*>) = ap
instance Monad CountBinds where
  (CountBinds (count1, item)) >>= func =
    let CountBinds (count2, processed) = func item in
      CountBinds (count1 + count2 + 1, processed)
  return item = CountBinds (0, item)

two :: CountBinds Int
two = do
  x <- return 1
  y <- return 2
  return $ x + y

main :: IO ()
main = do
  let a = 0 :: Integer
      k = (\x -> Identity (x + 1))
      m = Identity (0 :: Integer)
      h = (\x -> Identity (x - 1))
    in
    do
      putStr $ show (return a >>= k)
      putStr " = "
      putStr $ show (k a)
      putStr "\n"
      putStr $ show (m >>= return)
      putStr " = "
      putStr $ show (m)
      putStr "\n"
      putStr $ show (m >>= (\x -> k x >>= h))
      putStr " = "
      putStr $ show ((m >>= k) >>= h)
      putStr "\n"
  let a = 0 :: Integer
      k = (\x -> CountBinds (42, x + 1))
      m = CountBinds (0, 0)
      h = (\x -> CountBinds (5, x - 1))
    in
    do
      putStr $ show (return a >>= k)
      putStr " = "
      putStr $ show (k a)
      putStr "\n"
      putStr $ show (m >>= return)
      putStr " = "
      putStr $ show (m)
      putStr "\n"
      putStr $ show (m >>= (\x -> k x >>= h))
      putStr " = "
      putStr $ show ((m >>= k) >>= h)
      putStr "\n"
      putStr "🚂 It was time for Thomas to leave. He has seen everything...\n💩\n"
