-- Leetcode 16. 3Sum Closest
--
-- Learn how to handle k Sum Closest

module Solution where

import Data.Function (on)
import Data.List (minimumBy, sort)

-- Efficient, recursive k-Sum Closest function
kSumClosest :: Int -> [Int] -> Int -> Either String Int
kSumClosest k nums target
  -- Pre-conditions: Ensure k is valid and there are enough elements.
  -- The list must be sorted for the two-pointer approach to work.
  | k < 2 = Left "k must be at least 2"
  | length nums < k = Left "Not enough elements in the list for the given k"
  | otherwise = Right $ go k (sort nums) target
  where
    go :: Int -> [Int] -> Int -> Int
    -- Base Case: When k is 2, we use the efficient two-pointer algorithm.
    go 2 ns t = twoSumClosest ns t
    -- Recursive Step for k > 2.
    -- This guard handles cases where the list is exhausted or too short.
    go currentK ns t | length ns < currentK = maxBound -- Sentinel value

    -- Use pattern matching (x:xs) to safely deconstruct the list.
    -- This is only attempted on non-empty lists that have passed the guard above.
    go currentK (x : xs) t =
      let bestSumForRest = go (currentK - 1) xs (t - x)
          sumWithX = x + bestSumForRest

          -- Now, consider the case where we *skip* the current element 'x'
          -- and find the best k-sum from the rest of the list.
          sumWithoutX = go currentK xs t
       in -- Return the better of the two options.
          if abs (t - sumWithX) < abs (t - sumWithoutX)
            then sumWithX
            else sumWithoutX

twoSumClosest :: [Int] -> Int -> Int
twoSumClosest nums target = go 0 (length nums - 1) (nums !! 0 + nums !! 1)
  where
    go l r closest
      | l >= r = closest
      | otherwise =
          let currentSum = nums !! l + nums !! r
              newClosest =
                if abs (target - currentSum) < abs (target - closest)
                  then currentSum
                  else closest
           in case compare currentSum target of
                GT -> go l (r - 1) newClosest
                LT -> go (l + 1) r newClosest
                EQ -> currentSum

main :: IO ()
main = do
  putStrLn "--- Comprehensive Tests for kSumClosest ---"

  -- Test 1: Basic case from LeetCode
  let nums1 = [-1, 2, 1, -4]
  let target1 = 1
  putStrLn $ "Test 1: nums = " ++ show nums1 ++ ", k = 3, target = " ++ show target1
  putStrLn $ "  Expected: 2, Got: " ++ show (kSumClosest 3 nums1 target1)
  putStrLn ""

  -- Test 2: All same elements
  let nums2 = [0, 0, 0]
  let target2 = 1
  putStrLn $ "Test 2: nums = " ++ show nums2 ++ ", k = 3, target = " ++ show target2
  putStrLn $ "  Expected: 0, Got: " ++ show (kSumClosest 3 nums2 target2)
  putStrLn ""

  -- Test 3: 4-Sum case
  let nums3 = [1, 2, 3, 4, 5]
  let target3 = 11
  putStrLn $ "Test 3: nums = " ++ show nums3 ++ ", k = 4, target = " ++ show target3
  putStrLn $ "  Expected: 10, Got: " ++ show (kSumClosest 4 nums3 target3)
  putStrLn ""

  -- Test 4: Negative numbers and zero target
  let nums4 = [-5, -3, -1, 0, 2, 4]
  let target4 = 0
  putStrLn $ "Test 4: nums = " ++ show nums4 ++ ", k = 3, target = " ++ show target4
  putStrLn $ "  Expected: -1, Got: " ++ show (kSumClosest 3 nums4 target4) -- (-3 + 0 + 2 = -1)
  putStrLn ""

  -- Test 5: Duplicates and target is far
  let nums5 = [1, 1, 1, 1]
  let target5 = 100
  putStrLn $ "Test 5: nums = " ++ show nums5 ++ ", k = 3, target = " ++ show target5
  putStrLn $ "  Expected: 3, Got: " ++ show (kSumClosest 3 nums5 target5)
  putStrLn ""

  -- Test 6: Closest sum is from largest elements
  let nums6 = [1, 2, 10, 20]
  let target6 = 35
  putStrLn $ "Test 6: nums = " ++ show nums6 ++ ", k = 3, target = " ++ show target6
  putStrLn $ "  Expected: 32, Got: " ++ show (kSumClosest 3 nums6 target6) -- (2 + 10 + 20 = 32)
  putStrLn ""

  -- Test 7: Minimum number of elements
  let nums7 = [10, 20, 30]
  let target7 = 55
  putStrLn $ "Test 7: nums = " ++ show nums7 ++ ", k = 3, target = " ++ show target7
  putStrLn $ "  Expected: 60, Got: " ++ show (kSumClosest 3 nums7 target7)
  putStrLn ""

  -- Test 8: Invalid k (k < 2)
  let nums8 = [1, 2, 3]
  let target8 = 10
  putStrLn $ "Test 8: Invalid k (k=1)"
  putStrLn $ "  Expected: Left \"k must be at least 2\", Got: " ++ show (kSumClosest 1 nums8 target8)
  putStrLn ""

  -- Test 9: Not enough elements for k
  let nums9 = [1, 2, 3]
  let target9 = 10
  putStrLn $ "Test 9: Not enough elements (k=4, length=3)"
  putStrLn $ "  Expected: Left \"Not enough elements. Need 4, but have 3\", Got: " ++ show (kSumClosest 4 nums9 target9)
  putStrLn ""
