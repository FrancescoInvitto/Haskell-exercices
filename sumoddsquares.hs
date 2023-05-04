{- Sum of odd squares -
Sums all the odd squares that are smaller than 10000.
-}

sumOddSquares :: (Integral a) => a
sumOddSquares =  sum(takeWhile(<10000) (filter odd (map (^2)  [1..])))