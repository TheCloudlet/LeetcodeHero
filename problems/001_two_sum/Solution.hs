-- Leetcode 1. Two Sum
-- Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.

import qualified Data.Map as Map
import Text.Printf (printf)

-- |
-- Two Sum Algorithm using recursive approach:
-- 1. Iterate through the list with indices
-- 2. For each element, calculate its complement (target - current element)
-- 3. Check if complement exists in our seen-elements map
-- 4. If found, return the stored index and current index
-- 5. If not found, store current element and its index in the map for
--    future lookups
--
-- Time Complexity: O(n)
-- Space Complexity: O(n)
twoSum :: [Int] -> Int -> [Int]
twoSum nums target = go nums 0 Map.empty
  where
    -- Base case: reached end of list without finding solution
    go [] _ _ = []
    -- Process current element x at index i, with map m of seen elements
    go (x : xs) i m =
      case Map.lookup (target - x) m of
        Just j -> [j, i] -- Found complement at index j, return solution
        -- Store current element and continue searching
        Nothing -> go xs (i + 1) (Map.insert x i m)

-- |
-- Two Sum Algorithm using fold approach:
-- Uses foldl to accumulate both the result and seen-elements map.
-- The accumulator is a tuple: (result, seen_map, current_index)
--
-- Time Complexity: O(n)
-- Space Complexity: O(n)
twoSumFold :: [Int] -> Int -> [Int]
twoSumFold nums target =
  let (result, _, _) = foldl step ([], Map.empty, 0) nums
   in result
  where
    step ([], m, i) x =
      case Map.lookup (target - x) m of
        Just j -> ([j, i], m, i + 1) -- Found solution, store it
        Nothing -> ([], Map.insert x i m, i + 1) -- Continue searching
    step acc@(result, _, _) _ = acc -- Already found result, keep it

-- =============================================================================
-- TESTING FRAMEWORK
-- =============================================================================

data TestCase a b = TestCase
  { name :: String, -- A descriptive name for the test
    input :: a, -- The input to the function
    expected :: b -- The expected output
  }

testCases :: [TestCase ([Int], Int) [Int]]
testCases =
  [ TestCase "Example 1: [2,7,11,15], target=9" ([2, 7, 11, 15], 9) [0, 1],
    TestCase "Example 2: [3,2,4], target=6" ([3, 2, 4], 6) [1, 2],
    TestCase "Example 3: [3,3], target=6" ([3, 3], 6) [0, 1],
    TestCase "Negative numbers" ([-1, -2, -3, -4, -5], -8) [2, 4],
    TestCase "Zero in array" ([0, 4, 3, 0], 0) [0, 3],
    TestCase "Large numbers" ([1000000, 2000000, 3000000], 5000000) [1, 2],
    TestCase "Two elements only" ([1, 2], 3) [0, 1],
    TestCase "First and last" ([5, 1, 3, 9], 14) [0, 3]
  ]

-- Helper function to run a single test case for Two Sum.
-- Takes a Two Sum function and a test case, extracts the nums and target
-- from the input tuple, and runs the test.
runTwoSumTest :: ([Int] -> Int -> [Int]) -> TestCase ([Int], Int) [Int] -> IO ()
runTwoSumTest twoSumFunc tc = do
  let (nums, target) = input tc
      actual = twoSumFunc nums target
  if actual == expected tc
    then printf "\x1b[32m[PASS]\x1b[0m %s\n" (name tc) -- Green for PASS
    else do
      printf "\x1b[31m[FAIL]\x1b[0m %s\n" (name tc) -- Red for FAIL
      printf "       Input:    nums=%s, target=%d\n" (show nums) target
      printf "       Expected: %s\n" (show $ expected tc)
      printf "       Actual:   %s\n" (show actual)

main :: IO ()
main = do
  putStrLn "-----------------------------------------"
  putStrLn "Running tests for Two Sum ..."
  putStrLn "-----------------------------------------"
  -- Test both implementations
  putStrLn "Testing twoSum (recursive):"
  mapM_ (runTwoSumTest twoSum) testCases
  putStrLn "\nTesting twoSumFold:"
  mapM_ (runTwoSumTest twoSumFold) testCases
  putStrLn "-----------------------------------------"
  putStrLn "Tests complete."
