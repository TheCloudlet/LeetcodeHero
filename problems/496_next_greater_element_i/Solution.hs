-- \| Leetcode 496. Next Greater Element I
-- \|
-- \| Given nums1 (subset of nums2), find for each element in nums1 the next
-- \| greater element in nums2. If none, return -1.
-- \|
-- \| Approach:
-- \| 1. Use a monotonic decreasing stack to process nums2. When we find a
-- \|    greater value, pop stack and map it.
-- \|
-- \| 2. Build a map from each number to its next greater value.
-- \| 3. Lookup nums1 from that map. If not found, return -1.
-- \|
-- \| Note: Logic works, but stack pop logic is still a bit tricky.
-- \|       Needs more practice to fully internalize.

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

buildNextGreaterMap :: [Int] -> M.Map Int Int
buildNextGreaterMap nums = go nums [] M.empty
  where
    go [] _ acc = acc
    go (x : xs) stack acc =
      let (newStack, newAcc) = popStack x stack acc
       in go xs (x : newStack) newAcc

    popStack _ [] acc = ([], acc)
    popStack x (s : ss) acc
      | s < x = popStack x ss (M.insert s x acc)
      | otherwise = (s : ss, acc)

-- | Query phase: map lookup with default -1
nextGreaterElement :: [Int] -> [Int] -> [Int]
nextGreaterElement nums1 nums2 =
  let nextMap = buildNextGreaterMap nums2
   in map (\x -> fromMaybe (-1) (M.lookup x nextMap)) nums1

main :: IO ()
main = do
  let nums1 = [4, 1, 2]
      nums2 = [1, 3, 4, 2]
  print $ nextGreaterElement nums1 nums2 -- Expected: [-1,3,-1]
