module FileOperations where

import Prelude

import Data.Path (Path, root, ls, isDirectory, filename, size)
import Data.Array (concatMap, (:), (..), head, null, length, filter)
import Data.Array.Partial as A
import Data.Foldable (foldl)
import Data.Maybe (Maybe)
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

-- Exercise solutions for chapter 4

-- Ch. 4.1-4.4

-- Exercise 1.
isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven $ n - 2

-- Exercise 2.
countEven :: Array Int -> Int
countEven = length <<< filter isEven

-- Ch. 4.5-4.7

-- Exercise 1.
mapSquares :: Array Int -> Array Int
mapSquares = map (\n -> n * n)

-- Exercise 2.
isPositive :: Int -> Boolean
isPositive i = i >=0

filterPositives :: Array Int -> Array Int
filterPositives = filter isPositive

-- Exercise 3.
infix 8 filter as <$?>

filterPositivesAlt :: Array Int -> Array Int
filterPositivesAlt arr = isPositive <$?> arr


-- Ch. 4.8-4.11

-- Exercise 1.
factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime = (\l -> length l == 1) <<< factors

-- Exercise 2.
cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct x y = do
  a <- x
  b <- y
  pure [a, b]

-- Exercise 3.
triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [a, b, c]

-- Exercise 4.
factorizations :: Int -> Array (Array Int)
factorizations 1 = [[1]]
factorizations n = do
  x <- 2 .. n
  guard $ mod n x == 0
  y <- factorizations $ n / x
  pure $ x : y


-- Ch. 4.12-4.15

-- Exercise 1.
allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

-- Exercise 2.
-- foldl (==) false xs
--  Returns true only for Arrays of type (Array Boolean)
--  with and odd number of 'false' values

-- Exercise 3.
count :: forall a. (a -> Boolean) -> Array a -> Int
count = count' 0
  where
    count' s _ [] = s
    count' s p xs =
      if p (unsafePartial A.head xs)
        then count' (s + 1) p (unsafePartial A.tail xs)
        else count' s p (unsafePartial A.tail xs)

-- Exercise 4.
-- "array reverse using foldl"
reverseWithFoldLeft :: forall a. Array a -> Array a
reverseWithFoldLeft = foldl (\acc x -> [x] <> acc) []


-- Ch. 4.16-4.17

-- Exercise 1.
onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles

-- Exercise 2.

smallestFile :: Path -> Path
smallestFile root = foldl selectSmaller root $ onlyFiles root
  where
  selectSmaller :: Path -> Path -> Path
  selectSmaller x y = if size x < size y then x else y

largestFile :: Path -> Path
largestFile root = foldl selectLarger root $ onlyFiles root
  where
  selectLarger :: Path -> Path -> Path
  selectLarger x y = if size x > size y then x else y

-- Exercise 3.
whereIs :: String -> Maybe Path
whereIs str = head $ do
    path <- allFiles' root
    childPath <- ls path
    guard $ filename childPath == filename path <> str
    pure path
