{- Skyscrapers with fold -
Returns the number of times the maximum changes, from left to right, in a list. Uses the fold function.
-}

countTops :: [Int] -> Int
countTops l = snd (foldl (\(maxVal, count) x -> if x > maxVal then (x, count + 1) else (maxVal, count)) (0, 0) l)
