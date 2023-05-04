import Data.Bits (shiftL, shiftR, xor)
import Data.List
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32)

type Rng32 = Word32

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

launchDice :: Rng32 -> [Int]
launchDice gen =
  let (firstLaunch, newGen) = randint (1, 6) gen
      (secondLaunch, newGen') = randint (1, 6) newGen
      (thirdLaunch, newGen'') = randint (1, 6) newGen'
   in [firstLaunch, secondLaunch, thirdLaunch]

playGames :: Int -> Rng32 -> [Int]
playGames n gen = map countGreater tripletLists1 tripletLists2
  where
    (rolls1, rolls2) = launch n gen ([], [])
    tripletLists1 = sortValuesDescending $ divideInTriplets rolls1
    tripletLists2 = sortValuesDescending $ divideInTriplets rolls2

    countGreater xs ys = length (filter (\(x, y) -> x > y) (zip xs ys))

    launch 0 _ (rolls1, rolls2) = (rolls1, rolls2)
    launch n g (rolls1, rolls2) =
      let (values1, values2) = (launchDice g, launchDice (-g))
      in launch (n - 1) g (rolls1 ++ values1, rolls2 ++ values2)

{-launchDiceNTimes :: Int -> Rng32 -> [Int]
launchDiceNTimes n gen = map countGreater (sortValuesDescending (divideInTriplets (launch n gen ([], []))))
  where
    launch 0 _ (rolls1, rolls2) = (rolls1, rolls2)
    launch n g (rolls1, rolls2) =
      let (value1, value2) = (launchDice g, launchDice (-g))
       in launch (n - 1) g (rolls1 ++ value1, rolls2 ++ value2)
    g = getRng32 -}
    
{-launchDiceNTimes :: Int -> Rng32 -> [Int]
launchDiceNTimes n gen = map countGreater tripletLists1 tripletLists2
  where
    (tripletLists1, tripletLists2) = sortValuesDescending $ divideInTriplets $ launch n gen ([], [])

    launch 0 _ (rolls1, rolls2) = (rolls1, rolls2)
    launch n g (rolls1, rolls2) =
      let (value1, value2) = (launchDice g, launchDice (-g))
      in launch (n - 1) g (rolls1 ++ [value1], rolls2 ++ [value2])

    countGreater xs ys = length (filter (\(x, y) -> x > y) (zip xs ys))-}

{-
launchDies :: Rng32 -> Rng32 -> ([Int], [Int])
launchDies gen gen' = (reverse (sort (take 3 (randints (1, 6) gen))), reverse (sort (take 3 (randints (1, 6) gen')))) -}

{-launchDies :: Rng32  ->  ([Int], [Int])
launchDies g = (reverse (sort (take 3 (randints (1, 6) g)), reverse (sort (take 3 (randints (1, 6) -g)))-}

{-playGame :: [Int]
playGame = foldl (\acc (x,y) -> if x > y then acc + 1 else acc) 0 (zip xs ys)
  where (xs, ys) = launchDies 32 44 3-}

{-playGame :: ([Int], [Int]) -> [Int]
playGame (xs, ys) = map countGreater (zip3 xs (drop 1 xs) (drop 2 xs))
  where
    countGreater (x, y, z) = length (filter (\(a, b) -> a > b) (zip [x, y, z] ys)) -}

{-playGames :: [[Int]] -> [[Int]] -> [Int]
playGames xs ys = map countGreater (zip xs ys)
  where
    countGreater (xs, ys) = length $ filter (\(a, b) -> a > b) (zip xs ys)-}

{-launchDiesNTimes :: Rng32 -> Rng32 -> Int -> [([Int], [Int])]
launchDiesNTimes gen gen' n = drop 1 (take (n + 1) (iterate (\(xs, ys) -> launchDies gen gen') ([], [])))-}

{-divideInTriplets :: [Int] -> [[Int]]
divideInTriplets [] = []
divideInTriplets xs = take 3 xs : divideInTriplets (drop 3 xs)-}

divideInTriplets :: ([Int], [Int]) -> ([[Int]], [[Int]])
divideInTriplets (xs, ys) = (divide xs, divide ys)
  where
    divide [] = []
    divide zs = take 3 zs : divide (drop 3 zs)

{-sortValuesDescending :: [[Int]] -> [[Int]]
sortValuesDescending = map (reverse . sort)-}

sortValuesDescending :: ([[Int]], [[Int]]) -> ([[Int]], [[Int]])
sortValuesDescending (xs, ys) = (sortDescending xs, sortDescending ys)
  where
    sortDescending = map (reverse . sort)

launchDiesNTimes :: Rng32 -> Rng32 -> Int -> ([Int], [Int])
launchDiesNTimes gen gen' n = (take (3 * n) (randints (1, 6) gen), (take (3 * n) (randints (1, 6) gen')))

countGreater :: [Int] -> [Int] -> Int
countGreater xs ys = length (filter (\(x, y) -> x > y) (zip xs ys))

{-countGreaterNTimes :: Rng32 -> Rng32 -> Int -> [Int]
countGreaterNTimes gen gen' n = map countGreater (launchDiesNTimes gen gen' n)-}