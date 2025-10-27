{-
Leetcode 34 - List version without Data.Vector

Binary search to find first and last index of target
Returns (-1, -1) if target is not found

WARNING: (!!) is O(n) in Haskell because lists are linked lists.
Use with care in binary search, as it may lead to O(n log n) total time.
Consider using Data.Vector for O(1) indexing if performance matters.
-}

searchRange :: [Int] -> Int -> (Int, Int)
searchRange xs target
  | null xs = (-1, -1)
  | otherwise =
      let l = lowerBound xs target
          r = upperBound xs target
       in if l <= r && r < length xs && xs !! l == target && xs !! r == target
            then (l, r)
            else (-1, -1)

-- | Find the first index where xs[i] >= target
lowerBound :: [Int] -> Int -> Int
lowerBound xs target = go 0 (length xs)
  where
    go low high
      | low < high =
          let mid = low + (high - low) `div` 2
           in if xs !! mid < target
                then go (mid + 1) high
                else go low mid
      | otherwise = low

-- | Find the last index where xs[i] <= target
upperBound :: [Int] -> Int -> Int
upperBound xs target = go 0 (length xs)
  where
    go low high
      | low < high =
          let mid = low + (high - low) `div` 2
           in if xs !! mid <= target
                then go (mid + 1) high
                else go low mid
      | otherwise = low - 1

main :: IO ()
main = do
  print $ searchRange [5, 7, 7, 8, 8, 10] 8 -- (3,4)
  print $ searchRange [5, 7, 7, 8, 8, 10] 6 -- (-1,-1)
  print $ searchRange [1, 2, 2, 2, 3] 2 -- (1,3)
  print $ searchRange [2, 2, 2, 2] 2 -- (0,3)
  print $ searchRange [1, 3, 5] 4 -- (-1,-1)
