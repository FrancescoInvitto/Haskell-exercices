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