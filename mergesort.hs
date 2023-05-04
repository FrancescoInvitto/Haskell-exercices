{- Merge -
Takes two sorted lists and returns a sorted list with all the elements.
-}

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
  | x < y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys

{- Halve -
Splits a list into two halves.
-}

halve :: [a] -> ([a], [a])
halve l = (take half l, drop half l)
        where half = (length l `div` 2)

{- MergeSort -
Takes a list, splits it at half, sorts each part recursively and merges the two sorted parts.
-}

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort list = merge (mergesort left) (mergesort right)
  where (left, right) = halve list

