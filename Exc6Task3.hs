module Main where

data Format = Flushleft | Flushright | Center | Justify deriving (Eq)

lorem = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."

justify :: Format -> Int -> String -> String
justify format width =
  let
    member_length :: [String] -> Int
    member_length words = length words + (sum . map length) words

    group_until_length :: Int -> [[String]] -> String -> [[String]]
    group_until_length width acc x
      | acc == [] = [[x]]
      | (member_length (last acc)) <= (width - (length x)) = (init acc) ++ [(last acc) ++ [x]]
      | otherwise = acc ++ [[x]]

    block :: [String] -> Int -> [String]
    block ws chars=
      let
        wc = length ws - 1
        missing = width - chars
      in
        let pad_ws = (map
                      (
                        \(i, w) ->
                          if i < missing && i < wc
                          then w ++ " "
                          else w
                      ) . zip [0..]
                     ) ws
        in
          if (member_length pad_ws - 1) == width
          then pad_ws
          else block pad_ws (member_length pad_ws - 1)

    pad :: Format -> Int -> [String] -> [String]
    pad Flushright width string = (map (\x -> (replicate (width - (length x)) ' ') ++ x)) string
    pad Center width string = (map (\x -> (replicate ((width - (length x)) `quot` 2) ' ') ++ x)) string
    pad Flushleft _ string = string
    pad Justify width string = map (\line -> unwords $ block (words line) (length line)) string
  in
    unlines .
    pad format width .
    map unwords .
    foldl (group_until_length width) [] .
    words

main :: IO ()
main = do
  putStr $ justify Flushleft 5 "ab cd edfghij klm"
  putStr $ justify Flushleft 40 lorem
  putStr $ justify Flushright 40 lorem
  putStr $ justify Center 40 lorem
  putStr $ justify Justify 40 lorem
