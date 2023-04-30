{- 
--- Haskell exercises ---
Course: Paradigmi e linguaggi per l'analisi dei dati
Author: Francesco Invitto
Edit: 30/04/2023
-}

import Control.Monad (when)
import Data.Bits (shiftL, shiftR, xor)
--import Data.List
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32)
import Data.List (transpose, nub)
import System.IO

type Rng32 = Word32

{- Average of 5 elements -
Returns the average of the first 5 values (if present) of a list.
-}

avg list = (sum list) / (fromIntegral (length list))
avg5 list = avg (take 5 list)

{- Pythagoras -
Returns the length of the sides of a triangle (as a tuple) that has:
  - all sides are integer
  - all sides are equal or smaller than 10
  - the perimeter of the triangle is 24
using a comprehension.
-}

triangle24perim = [(x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10], x + y + z == 24, z ^ 2 == x ^ 2 + y ^ 2]

--optimization
--triangle24perim = [(x, y, z) | x <- [1..10], y <- [x..10], z <- [y..10], x + y + z == 24, z ^ 2 == x ^ 2 + y ^ 2]

--further optimization
--triangle24perim = [(x, y, z) | x <- [1..10], y <- [x..10], z <- [24 - x - y], z ^ 2 == x ^ 2 + y ^ 2]

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

{- Reverse with tail recursion -
Implements the reverse function using tail recursion.
Reverts the elements of a list.
-}
reverse' :: [a] -> [a] -> [a]
reverse' [] acc = acc
reverse' (x:xs) acc  = reverse' xs (x:acc)

{- Sum of odd squares -
Sums all the odd squares that are smaller than 10000.
-}
sumOddSquares :: (Integral a) => a
sumOddSquares =  sum(takeWhile(<10000) (filter odd (map (^2)  [1..])))

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

{- Skyscrapers with fold -
Returns the number of times the maximum changes, from left to right, in a list. Uses the fold function.
-}
countTops' :: [Int] -> Int
countTops' l = snd (foldl (\(maxVal, count) x -> if x > maxVal then (x, count+1) else (maxVal, count)) (0, 0) l)

{- Phonebook -
Implements a lookup for a phonebook.
-}
type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

myPhoneBook = [("A", "123"), ("C", "456"), ("E", "789")]

-- with recursion
{-
getPhoneNumber :: Name -> PhoneBook -> PhoneNumber
getPhoneNumber "" _ = "No name specified"
getPhoneNumber name [] = ""
getPhoneNumber name (x : xs)
  | name == fst x = "Phone number for " ++ name ++ " is " ++ snd x
  | otherwise = getPhoneNumber name xs -}

-- with tail recursion
{-
getPhoneNumber' :: Name -> PhoneBook -> PhoneBook -> PhoneNumber
getPhoneNumber' "" _ _= "No name specified"
getPhoneNumber' name [] acc = "No phone number for " ++ name
getPhoneNumber' name ((n, p) : xs) acc
  | (name == n) = "Phone number for " ++ name ++ " is " ++ p
  | otherwise = getPhoneNumber' name xs ((n, p) : acc) -}

-- with fold
getPhoneNumber'' :: Name -> PhoneBook -> PhoneNumber
getPhoneNumber'' "" _  = "No name specified"
getPhoneNumber'' name phonebook = foldr (\(n, p) acc -> if n == name then found ++ p else acc) notFound phonebook
  where found = "Phone number for " ++ name ++ " is "
        notFound = "No phone number for " ++ name

--with maybe
getPhoneNumber''' :: Name -> PhoneBook -> Maybe PhoneNumber
getPhoneNumber''' "" _ = Nothing
getPhoneNumber''' name phonebook = foldr (\(n, p) acc -> if n == name then Just p else acc) Nothing phonebook

getPhoneNumber' :: Name -> PhoneBook -> PhoneBook -> Maybe PhoneNumber
getPhoneNumber' "" _ _= Nothing
getPhoneNumber' name [] acc = Nothing
getPhoneNumber' name ((n, p) : xs) acc
  | (name == n) = Just p
  | otherwise = getPhoneNumber' name xs ((n, p) : acc)

{- Bouncing ball -
Mimics a move function for a ball, for advancing a step and bouncing at borders.
-}
data Ball = Ball {
  x :: Int,
  y :: Int,
  dx :: Int,
  dy :: Int,
  w :: Int,
  h :: Int
} deriving (Show)

ball1 = Ball {x = 140, y = 180, dx = 5, dy = 5, w = 20, h = 20}
ball2 = Ball {x = 180, y = 140, dx = 5, dy = 5, w = 20, h = 20}

move :: Int -> Int -> Ball -> Ball
move aw ah (Ball x y dx dy w h)
  | not (0 <= (x + dx) && (x + dx) <= (aw - w)) = move aw ah (Ball x y (-dx) dy w h)
  | not (0 <= (y + dy) && (y + dy) <= (ah - h)) = move aw ah (Ball x y dx (-dy) w h)
  | otherwise = Ball (x + dx) (y + dy) dx dy w h

xorshift32 :: Rng32 -> Rng32
xorshift32 a = d
  where
    b = a `xor` (a `shiftL` 13)
    c = b `xor` (b `shiftR` 17)
    d = c `xor` (c `shiftL` 5)

randint :: (Int, Int) -> Rng32 -> (Int, Rng32)
randint (nmin, nmax) gen = (val, nxt)
  where
    nxt = xorshift32 gen
    val = nmin + (fromIntegral nxt) `mod` (nmax + 1 - nmin)

randints :: (Int, Int) -> Rng32 -> [Int]
randints range gen =
  val : randints range nxt
  where
    (val, nxt) = randint range gen

getRng32 :: IO Rng32
getRng32 = do
  now <- getPOSIXTime
  return (round (now * 1000) :: Rng32)

main = do
  gen <- getRng32
  let number = fst (randint (1, 90) gen)
  guess number 10

guess :: Int -> Int -> IO ()
guess number attempts = do
  if attempts > 0
  then do
    putStrLn "Guess the number (1-90): "
    choice <- getLine
    if not $ null choice
      then do
        if choice == show number
        then putStrLn ("Correct, the number was " ++ show number ++ ". You guessed it in " ++ show (10-attempts+1) ++ " attempts.")
        else do
          if choice < show number
          then putStrLn ("Wrong! The number is bigger. Attempts left: " ++ show (attempts - 1))
          else putStrLn ("Wrong! The number is smaller. Attempts left: " ++ show (attempts - 1))
          guess number (attempts - 1)
      else do
        putStrLn "Please insert a number."
        guess number attempts
  else putStrLn ("No more attempts! The number was " ++ show number)

{- Sudoku Skycrapers -
Asks a file name and reads the game from it. Then it checks if the solution is correct or not. In particular the rules are:
  - all the lines/columns must contain all the numbers in the interval [1, gridSize]
  - all the lines/columns must contain all unique numbers (automatically true if the previous holds)
  - all the lines/columns must satisfy the constraints indicated at the borders.
-}

-- reads the file and returns the list of lists of int and the size of the grid
readIntListFile :: FilePath -> IO ([[Int]], Int)
readIntListFile filePath = do
  game <- readFile filePath
  putStrLn ("\nGame read: \n\n" ++ game)
  let linesOfFile = lines game
      intLists = map (map read . words) linesOfFile
  return (intLists, length intLists - 2)

-- checks if a line/column contains all the numbers between 1 and the size of the grid
containsAllNumbers :: [Int] -> Int -> IO [Bool]
containsAllNumbers x size = return $ map (`elem` init (tail x) ) [1 .. size]

-- checks if a line/columns contains each number only once (REDUNDANT)
--onlyOnce :: [Int] -> Bool
--onlyOnce x = length (init (tail x)) == length (nub (init (tail x)))

-- checks if the number of visible tops on each line/column (in both directions) satisfies the constraints
checkVisibleTops :: [Int] -> Bool
checkVisibleTops x
  | constr == 0 = True -- there is no constraint
  | otherwise = countTops' (init (tail x)) == constr
  where constr = head x
  
main' = do
  putStrLn "Insert the name of the file containing the game you want to check: "
  name <- getLine
  if not $ null name
  then do
    (lns, gridSize) <- readIntListFile name
    -- print lns

    putStrLn ("\nThe grid is " ++ show gridSize ++ "x" ++ show gridSize ++ "\n")

    let cols = transpose lns
    -- print cols

    -- check of constraints
    putStrLn "--------------- Check of rules ---------------"
  
    let lineAllNumChecks = map (\i -> containsAllNumbers (lns !! (i)) gridSize) [1 .. gridSize]
    allNumChecks <- sequence lineAllNumChecks
    let lineTrue = and (concat allNumChecks)
    putStrLn ("All lines contain all numbers: " ++ show lineTrue)
    putStrLn ("All lines contain all unique numbers: " ++ show lineTrue)
  
    let colAllNumChecks = map (\i -> containsAllNumbers (cols !! (i)) gridSize) [1 .. gridSize]
    allNumChecks <- sequence colAllNumChecks
    let colTrue = and (concat allNumChecks)
    putStrLn ("All columns contain all numbers: " ++ show colTrue)
    putStrLn ("All columns contain all unique numbers: " ++ show colTrue)
  
    {-let lineAllUniqueChecks = map (\i -> onlyOnce (lns !! i)) [1 .. gridSize]
    allUniqueChecks <- sequence lineAllUniqueChecks
    let allUnique = and lineAllUniqueChecks
    putStrLn ("All lines contain all unique numbers: " ++ show allTrue)
  
    let colAllUniqueChecks = map (\i -> onlyOnce (cols !! i)) [1 .. gridSize]
    --allUniqueChecks <- sequence colAllUniqueChecks
    let allUnique = and colAllUniqueChecks
    putStrLn ("All columns contain all unique numbers: " ++ show allTrue)
    --print (lns ++ (map reverse lns))-}
    
    let lineSatisfConstr = and (map (\l -> checkVisibleTops l) ((init (tail lns)) ++ (map reverse (init (tail lns)))))
    putStrLn ("All lines satisfy the constraints: " ++ show lineSatisfConstr)
  
    let colSatisfConstr = and (map (\l -> checkVisibleTops l) ((init (tail cols)) ++ (map reverse (init (tail cols)))))
    putStrLn ("All columns satisfy the constraints: " ++ show colSatisfConstr)
    putStrLn "----------------------------------------------"

    if lineTrue && colTrue && lineSatisfConstr && colSatisfConstr
    then putStrLn "The game solution is correct."
    else putStrLn "The game solution is not correct."
  else do
    putStrLn "Please insert a file name."
    main'
