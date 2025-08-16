-- Leetcode 217. Contains Duplicate

module Solution where

import qualified Data.Set as Set

containsDuplicate :: (Ord a) => [a] -> Bool
containsDuplicate x = go Set.empty x
  where
    go _ [] = False
    go seen (x : xs)
      | Set.member x seen = True
      | otherwise = go (Set.insert x seen) xs

-- Helper function to format and print the test case and its result.
-- It requires 'Show a' so it can print the list elements.
-- It requires 'Ord a' to pass the list to containsDuplicate.
testCase :: (Ord a, Show a) => [a] -> IO ()
testCase list = do
  let result = containsDuplicate list
  putStrLn $ show list ++ " --> " ++ show result

main :: IO ()
main = do
  -- Now the main function is much cleaner and easier to read.
  testCase [1, 2, 2, 3]
  testCase [1, 2, 3, 4]
  testCase ([] :: [Int]) -- Testing an empty list
  testCase ["hello", "world", "hello"] -- Works with Strings too!