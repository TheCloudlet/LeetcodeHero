-- Leetcode 1. Two Sum

import qualified Data.Map as Map

-- 1. Iterate all items in list
-- 2. Look up complement in map
-- 3. If not exsists add to map

twoSum :: [Int] -> Int -> [Int]
twoSum nums target = go nums 0 Map.empty
  where
    go [] _ _ = [] -- not found
    go (x : xs) i m =
      case Map.lookup (target - x) m of
        Just j -> [j, i] -- found the complement
        Nothing -> go xs (i + 1) (Map.insert x i m)

main :: IO ()
main = do
  print $ twoSum [2, 7, 11, 15] 9 -- [0,1]
  print $ twoSum [3, 2, 4] 6 -- [1,2]
  print $ twoSum [3, 3] 6 -- [0,1]
