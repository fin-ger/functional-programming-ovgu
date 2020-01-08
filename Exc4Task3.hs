import Data.List

mySplitAt :: Int -> String -> [String]
mySplitAt num string = splitAt' num string "" where
  -- lets split at this index
  splitAt' 0 string split = [split, string]
  -- build split
  splitAt' num (x:xs) split = splitAt' (num - 1) xs (split ++ [x])

-- [2,4,6,9] becomes [2,2,2,3] because we need it for splitAtMultiple'
toOffsets :: [Int] -> [Int]
toOffsets i = toOffsets' (sort i) 0 [] where
  toOffsets' [] _ result = result
  toOffsets' (x:xs) offs result = toOffsets' xs x (result ++ [x - offs])

splitAtMultiple :: [Int] -> String -> [String]
splitAtMultiple splits string = splitAtMultiple' (toOffsets splits) string [] where
  -- if there are no more splits, return the result with the rest
  splitAtMultiple' [] string result = result ++ [string]
  splitAtMultiple' (s:splits) string result
    -- report index error if index is out of range
    | s > length string =
      error ("index " ++ show (s + sum (map length result)) ++ " is out of range")
    -- split string at each split
    | otherwise =
        let [split, tail] = mySplitAt s string in
          splitAtMultiple' splits tail (result ++ [split])
