import Control.Monad (when)
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

{- Guess the number -
Generates a random number between 1 and 90 and then asks to the user to guess it in at most 10 attempts.
-}
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
      