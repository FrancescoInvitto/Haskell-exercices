{- Max of two values -
Returns the maximum between the two passed values.
-}

maxOfTwo :: (Ord a) => a -> a -> a
maxOfTwo a b
  | a < b = b
  | otherwise = a

{- Calculator -
Parameters: an operator and two numbers.
The operator must be any of "+, -, *, /".
Returns the result of the operation as a string.
-}

calculate :: (Num a, Show a, Eq a, Fractional a) => String -> a -> a -> String
calculate "+" a b = show (a + b)
calculate "-" a b = show (a - b)
calculate "*" a b = show (a * b)
calculate "/" _ 0 = "Not possible to divide by 0!"
calculate "/" a b = show (a / b)
calculate _ a b = "Operation not supported!"