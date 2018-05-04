module Exercises where

import Prelude
import Data.Array ((..))
import Data.Maybe
import Data.Picture
import Math (pi)

-- Exercise solutions for chapter 5.


-- Ch. 5.1-5.5

-- Exercise 1.

factorial :: Int -> Int
factorial a = factorial' a 1
  where
  factorial' :: Int -> Int -> Int
  factorial' 0 n = n
  factorial' n p = factorial' (n-1) (p*n)

factorialTail :: Int -> Int
factorialTail 0 = 1
factorialTail n = n * factorialTail(n-1)

-- Exercise 2.
binomialCoeffs :: Int -> Array Int
binomialCoeffs 0 = []
binomialCoeffs n = [1] <> do
  k <- 1..n
  pure (coeff n k)
  where
    coeff :: Int -> Int -> Int
    coeff 1 b = 0
    coeff a 1 = a
    coeff a b = (coeff (a-1) (b-1)) + (coeff (a-1) b)


-- Ch. 5.6-5.9

-- Exercise 1.

type Address = { street :: String, city :: String }

type Person = { name :: String, address :: Address }

sameCity :: { address :: { city :: String }} -> { address :: { city :: String } } -> Boolean
sameCity { address: { city: a } } { address: { city: b } } = a == b

-- Exercise 2.

sameCity' :: forall r s. { address :: { city :: String | s } | r } -> { address :: { city :: String | s } | r } -> Boolean
sameCity' { address: { city: a } } { address: { city: b } } = a == b

livesInLA' :: forall r s. { address :: { city :: String | s } | r } -> Boolean
livesInLA' { address: { city: "Los Angeles" } } = true
livesInLA' _ = false

-- Exercise 3.
fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton d _ = d


-- Ch. 5.10-5.14

-- Exercise 1.
centeredCircle :: Shape
centeredCircle = Circle (Point { x, y }) 10.0
  where
  x = 0.0
  y = 0.0

-- Exercise 2.

origin :: Point
origin = Point { x: 0.0, y: 0.0 }

scaleByTwoAndCenter :: Shape -> Shape
scaleByTwoAndCenter (Circle _ r) = Circle origin (2.0 * r)
scaleByTwoAndCenter (Rectangle _ w h) = Rectangle origin (2.0 * w) (2.0 * h)
scaleByTwoAndCenter (Line (Point { x: xa, y: ya }) (Point { x: xb, y: yb })) =
  Line (Point { x: -xdiff, y: -ydiff }) (Point { x: xdiff, y: ydiff })
  where
  xdiff = xb - xa
  ydiff = yb - ya
scaleByTwoAndCenter (Text _ str) = Text origin str
scaleByTwoAndCenter (Clipped pic shape) = Clipped pic (scaleByTwoAndCenter shape) -- Dummy implementation

-- Exercise 3.

textFromShape :: Shape -> Maybe String
textFromShape (Text _ str) = Just str
textFromShape _ = Nothing


-- Ch. 5.15-5.17

-- Exercise 1.
area :: Shape -> Number
area (Circle _ r) = pi * r * r
area (Rectangle _ w h) = w * h
area (Line _ _) = 0.0
area (Text _ _) = 0.0
area (Clipped _ shape) = area shape

-- Exercise 2.
-- Implemented into existing defitions in this file and Data/Picture.purs
