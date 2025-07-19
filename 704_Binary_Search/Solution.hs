-- Leetcode 704 - Binary Search (Haskell Version)
-- Difficulty: Easy
-- Goal: Return index of target in sorted array, or -1 if not found.
search :: [Int] -> Int -> Int
search nums target = go 0 (length nums - 1) where
  go :: Int -> Int -> Int
  go left right
    | left > right = -1 -- base case: not found
    | nums !! mid == target = mid
    | nums !! mid < target = go (mid + 1) right
    | otherwise = go left (mid - 1)
    where mid = left + (right - left) `div` 2

main :: IO()
main = do
  print$ search [1,2,3,4,5] (-2)
  print $ search [1,2,3,4,5] 1
  print $ search [1,2,3,4,5] 3
  print $ search [1,2,3,4,5] 4
  print $ search [1,2,3,4,5] 7
