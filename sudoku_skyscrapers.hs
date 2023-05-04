import Data.List (transpose, nub)
import System.IO

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

-- counts the number of visible skyscrapers
countTops :: [Int] -> Int
countTops l = snd (foldl (\(maxVal, count) x -> if x > maxVal then (x, count+1) else (maxVal, count)) (0, 0) l)

-- checks if the number of visible tops on each line/column (in both directions) satisfies the constraints
checkVisibleTops :: [Int] -> Bool
checkVisibleTops x
  | constr == 0 = True -- there is no constraint
  | otherwise = countTops (init (tail x)) == constr
  where constr = head x
  
main = do
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
    main
