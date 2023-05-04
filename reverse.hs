{- Reverse with tail recursion -
Implements the reverse function using tail recursion.
Reverts the elements of a list.
-}

reverse' :: [a] -> [a] -> [a]
reverse' [] acc = acc
reverse' (x:xs) acc  = reverse' xs (x:acc)
