-- Leetcode 1. Two Sum
-- Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.

import qualified Data.Map as Map

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

main :: IO ()
main = do
  putStrLn "Testing recursive approach:"
  print $ twoSum [2, 7, 11, 15] 9
  print $ twoSum [3, 2, 4] 6
  print $ twoSum [3, 3] 6

  putStrLn "\nTesting fold approach:"
  print $ twoSumFold [2, 7, 11, 15] 9
  print $ twoSumFold [3, 2, 4] 6
  print $ twoSumFold [3, 3] 6
