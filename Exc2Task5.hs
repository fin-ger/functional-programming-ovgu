module Main where

import Text.Printf (printf)

main :: IO ()
main = do
  printf "'Foo' '' -> '%s'\n" (asPrefixOf "Foo" "")
  printf "'foo' 'barfoo' -> '%s'\n" (asPrefixOf "foo" "barfoo")
  printf "'foo' 'foobar' -> '%s'\n" (asPrefixOf "foo" "foobar")
  printf "'bar' 'bazbar' -> '%s'\n" (asPrefixOf "bar" "bazbar")
  printf "'foobar' 'obrbaz' -> '%s'\n" (asPrefixOf "foobar" "obrbaz")

asPrefixOf :: String -> String -> String
asPrefixOf p s = asPrefixOf' p s "" where
  asPrefixOf' :: String -> String -> String -> String
  asPrefixOf' p' s' r
    | length p' == 0 = r ++ s'
    | length s' == 0 = p' ++ r
    | head p' == head s' = asPrefixOf' (tail p') (tail s') (r ++ [head p'])
    | otherwise = asPrefixOf' (tail p') s' (r ++ [head p'])
