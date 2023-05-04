{- Collatz -
Returns the collatz chain for the number passed as parameter.
-}

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | even n = n : collatz (n `div` 2)
  | odd n = n : collatz (n * 3 + 1)

{- Collatz chains -
Returns the number of collatz chains with length greater than 15 for all the starting numbers between 1 and 100.
-}

collatzCountChains = length (filter longerThanFifteen (map collatz [1 .. 100]))
  where
    longerThanFifteen chain = length chain > 15