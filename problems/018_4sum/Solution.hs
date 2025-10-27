-- Leetcode 18. 4 Sum

-- Key: handle duplicate with `dropWhile`

module Solution where

import Data.List (sort)

fourSum :: [Int] -> Int -> Either String [[Int]]
fourSum nums target
  | length nums < 4 = Left "Input array should be more than 4"
  | otherwise = Right $ kSum 4 (sort nums) target

kSum :: Int -> [Int] -> Int -> [[Int]]
kSum 2 nums target = twoSum nums target
kSum k nums _
  | k < 2 || null nums = []
kSum k (x : xs) target =
  let solutionsWithX = map (x :) (kSum (k - 1) xs (target - x))
      solutionsWithoutX = kSum k (dropWhile (== x) xs) target
   in solutionsWithX ++ solutionsWithoutX

twoSum :: [Int] -> Int -> [[Int]]
twoSum nums target = go nums (reverse nums)
  where
    go [] _ = []
    go _ [] = []
    go ((li, l) : ls) ((ri, r) : rs)
      -- CORRECTED GUARD: Compare indices (`li`, `ri`), not values (`l`, `r`).
      -- This correctly determines if the pointers have crossed.
      | li >= ri = []
      | otherwise =
          let currentSum = l + r
           in case compare currentSum target of
                LT -> go ls ((ri, r) : rs)
                GT -> go ((li, l) : ls) rs
                -- When a match is found, skip all duplicates of the *values*.
                EQ -> [l, r] : go (dropWhile ((== l) . snd) ls) (dropWhile ((== r) . snd) rs)

main :: IO ()
main = do
  putStrLn "--- LeetCode 18: 4Sum Tests ---"

  -- Test Case 1: Standard example
  let nums1 = [1, 0, -1, 0, -2, 2]
  let target1 = 0
  putStrLn "\nTest Case 1: nums = [1, 0, -1, 0, -2, 2], target = 0"
  putStrLn "Expected: [[-2,-1,1,2],[-2,0,0,2],[-1,0,0,1]]"
  printResult $ fourSum nums1 target1

  -- Test Case 2: Duplicates
  let nums2 = [2, 2, 2, 2, 2]
  let target2 = 8
  putStrLn "\nTest Case 2: nums = [2, 2, 2, 2, 2], target = 8"
  putStrLn "Expected: [[2,2,2,2]]"
  printResult $ fourSum nums2 target2

  -- Test Case 3: No solution
  let nums3 = [1, 2, 3, 4]
  let target3 = 25
  putStrLn "\nTest Case 3: nums = [1, 2, 3, 4], target = 25"
  putStrLn "Expected: []"
  printResult $ fourSum nums3 target3

  -- Test Case 4: Input too small (triggers error)
  let nums4 = [1, 2, 3]
  let target4 = 6
  putStrLn "\nTest Case 4: nums = [1, 2, 3], target = 6"
  putStrLn "Expected: Error message"
  printResult $ fourSum nums4 target4

-- | Helper function to neatly print the Either result.
printResult :: Either String [[Int]] -> IO ()
printResult (Left err) = putStrLn $ "  Result: Error: " ++ err
printResult (Right result) = putStr "  Result: " >> print result
