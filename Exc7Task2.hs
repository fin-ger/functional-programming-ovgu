module LeftistHeap (
    LeftistHeap
  , empty
  , node
  , rank
  , merge
  , insert
  , viewMinimum
  , extractMinimum
  , toList
  , fromList
  ) where

data LeftistHeap a = Branch a (LeftistHeap a) (LeftistHeap a) | Leaf
  deriving (Show, Eq)

-- O(1)
empty :: Ord a => LeftistHeap a
empty = Leaf

-- O(height)
node :: Ord a => a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
node item left right = merge (insert item left) right

-- O(height)
rank :: Ord a => LeftistHeap a -> Int
rank (Branch _ _ right) = 1 + rank right
rank Leaf = 0

-- O(height)
merge :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
merge left Leaf = left
merge Leaf right = right
merge left@(Branch litem lleft lright) right@(Branch ritem rleft rright)
  | litem <= ritem = let new_right = merge lright right in
      if rank lleft >= rank new_right
      then Branch litem lleft new_right
      else Branch litem new_right lleft
  | otherwise = let new_right = merge left rright in
      if rank rleft >= rank new_right
      then Branch ritem rleft new_right
      else Branch ritem new_right rleft

-- O(height)
insert :: Ord a => a -> LeftistHeap a -> LeftistHeap a
insert item tree = merge (Branch item Leaf Leaf) tree

-- O(1)
viewMinimum :: Ord a => LeftistHeap a -> Maybe a
viewMinimum (Branch root _ _) = Just root
viewMinimum Leaf = Nothing

-- O(height)
extractMinimum :: Ord a => LeftistHeap a -> Maybe (a, LeftistHeap a)
extractMinimum (Branch root left right) = Just (root, merge left right)
extractMinimum Leaf = Nothing

-- O(nodes)
toList :: Ord a => LeftistHeap a -> [a]
toList tree = inorder tree []
  where
    inorder Leaf acc = acc
    inorder (Branch item left right) acc = inorder left (item:inorder right acc)

-- O(nodes) + O(height)
fromList :: Ord a => [a] -> LeftistHeap a
fromList = foldl (flip insert) empty

main :: IO ()
main = do
  let tree = node 'a' (node 'b' empty empty) (node 'x' (node 'y' empty empty) empty) in do
    print (tree)
    print (viewMinimum tree)
    print (extractMinimum tree)
    print (toList tree)

  print (fromList ['a', 'b', 'x', 'y'])
