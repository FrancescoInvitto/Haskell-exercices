countTops :: [Int] -> Int
countTops l = snd (foldl (\(maxVal, count) x -> if x > maxVal then (x, count+1) else (maxVal, count)) (0, 0) l)

