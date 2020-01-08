import Numeric.Natural

data Article desc = Article desc Natural
  deriving (Show)
type Storage desc = [Article desc]

data Product desc name = Product
  {
    name :: name,
    ingredients :: [Article desc]
  }
  deriving (Show)

contains :: Eq desc => Storage desc -> desc -> Natural
-- no articles -> 0
contains [] _ = 0
-- is the current article the one we are looking for?
-- if not, look into tail
contains (Article name amount:tail) article_name
  | name == article_name = amount
  | otherwise = contains tail article_name

store :: Eq desc => Storage desc -> desc -> Natural -> Storage desc
store storage name amount = store' storage name amount [] where
  store' (Article cname camount:tail) name amount result
    -- is the current article the same we are storing? let's increase the amount
    | cname == name = result ++ (Article name (amount + camount) : tail)
    -- we couldn't find the article in the storage, so let's add it
    | length tail == 0 = result ++ [Article cname camount, Article name amount]
    -- the current article was not the one we were looking for, let's look into tail
    | otherwise = store' tail name amount (result ++ [Article cname camount])

remove :: Eq desc => Storage desc -> desc -> Natural -> Storage desc
remove storage name amount = remove' storage name amount [] where
  remove' (Article cname camount:tail) name amount result
    -- when current article is the one we are removing, decrease amount
    | cname == name && amount < camount = result ++ (Article name (camount - amount) : tail)
    -- or delete completely depending on the provided amount
    | cname == name = result ++ tail
    -- when the article is not in the storage, just leave it
    | length tail == 0 = result ++ [Article cname camount]
    -- when the current article is not the one we were looking for, look into tail
    | otherwise = remove' tail name amount (result ++ [Article cname camount])

ingredients_available :: (Eq a, Eq b) => Product a b -> Storage a -> Bool
ingredients_available (Product _ ingredients) storage =
  ingredients_available' ingredients storage where
  -- when there are no ingredients, we always have them in our storage
  ingredients_available' [] storage = True
  -- when there is at least one ingredient, check if it is contained in the
  -- storage for at least the given amount and also check for other
  -- ingredients in tail
  ingredients_available' (Article name amount : tail) storage =
    contains storage name >= amount && ingredients_available' tail storage

remove_product :: (Eq a, Eq b) => Product a b -> Storage a -> Storage a
remove_product (Product _ ingredients) storage =
  remove_product' ingredients storage where
  -- when there are no ingredients left, the storage stays the same
  remove_product' [] storage = storage
  -- remove the ingredient from the storage and use the new storage for
  -- removing all other ingredients left in tail
  remove_product' (Article name amount : tail) storage =
    remove_product' tail (remove storage name amount)

produce :: (Eq a, Eq b) => [Product a b] -> Storage a -> Storage a
produce [] storage = storage
-- just do it for all products if available
produce (product:tail) storage =
  if ingredients_available product storage
  then produce tail (remove_product product storage)
  else produce tail storage
