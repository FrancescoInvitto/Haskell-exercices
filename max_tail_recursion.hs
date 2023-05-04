{- Max with tail recursion -
Implements the max function using tail recursion.
Returns the maximum value in a list.
-}

max_tail_recursion :: (Ord a) => [a] -> a
max_tail_recursion l = maxi l (head l)
  where
    maxi :: (Ord a) => [a] -> a -> a
    maxi [] acc = acc
    maxi (x : xs) acc
      | x > acc = maxi xs x
      | otherwise = maxi xs acc

{- Skyscrapers with tail recursion -
Returns the number of times the maximum changes, from left to right, in a list. Uses tail recursion.
-}

countTops [] acc = length acc
countTops (x : xs) [] = countTops xs [x]
countTops (x : xs) acc
  | x > head acc = countTops xs (x : acc)
  | otherwise = countTops xs acc