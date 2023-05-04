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

  
  