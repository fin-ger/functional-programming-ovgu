module Main where
assert :: (Eq a, Show a) => a -> a -> IO ()
assert a b = if a /= b
  then error ("assertion failed: " ++ (show a) ++ " != " ++ (show b))
  else print ("success: " ++ (show a) ++ " == " ++ (show b))

main :: IO ()
main = do
  -- Erläutern Sie den Typen (.) :: (b -> c) -> (a -> b) -> a -> c
  --                                   f           g        result
  -- result wendet a auf g an und das Erbenis von g wird auf f angewendet

  assert
    ((sum . map length) ["this", "is", "an", "array", "of", "string"])
    21

  assert
    ((sum . map (\(a,b) -> a + b) . zip [0,2..]) [1,3..11])
    66

--  let f = undefined :: b -> c1 -> c
-- *Main> :t (f .)
-- (f .) :: (a -> b) -> a -> c1 -> c
-- es wird eine Funktion (a -> b) erwartet, die ein a in ein b übersetzt, wobei ein a mithilfe dieser Funktion die Funktion (c1 -> c) als Rückgabe erzeugt

-- *Main> let g = undefined :: a -> a1 -> b
-- *Main> :t (. g)
-- (. g) :: ((a1 -> b) -> c) -> a -> c
-- Ein a wird mithilfe der Funktion (a1 -> b) über eine weitere Funktion (...) -> c ein c aus einem a erzeugt. g bildet also eine "Brücke" um ein c zu erzeugen.
