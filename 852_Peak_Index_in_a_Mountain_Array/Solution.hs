{-
Leetcode 852. peak index in a mountain array
binary search for peak in strictly increasing then decreasing array
-}

-- Generic binary search: find first index where f i == True
binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch f low high = go low high
  where
    go l r
      | l < r =
          let mid = (l + r) `div` 2
          in if f mid
               then go l mid
               else go (mid + 1) r
      | otherwise = l

peakIndexInMountainArray :: [Int] -> Int
peakIndexInMountainArray xs =
  binarySearch (\i -> xs !! i > xs !! (i + 1)) 0 (length xs - 1)

main :: IO ()
main = do
  print $ peakIndexInMountainArray [0, 1, 2, 3, 2, 1]     -- 3
  print $ peakIndexInMountainArray [0, 2, 4, 6, 5, 3, 1]  -- 3
  print $ peakIndexInMountainArray [1, 3, 5, 4, 2]        -- 2
  print $ peakIndexInMountainArray [0, 10, 5, 2]          -- 1
  print $ peakIndexInMountainArray [0, 1, 0]              -- 1
