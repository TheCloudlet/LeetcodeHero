-- Leetcode 416. Partition Equal Subset Sum

module Solution where

import qualified Data.Array as Array
import Data.List (foldl')
import qualified Data.Set as Set
import Text.Printf (printf)

-- Set Solution
canPartition :: [Int] -> Bool
canPartition nums =
  let sumAll = sum nums
      (target, remainder) = sumAll `divMod` 2
   in remainder == 0 && Set.member target finalSum
  where
    initialSum = Set.singleton 0
    finalSum = foldl' updateSums initialSum nums

updateSums :: Set.Set Int -> Int -> Set.Set Int
updateSums acc num =
  let newSums = Set.map (+ num) acc
   in Set.union acc newSums

-- DP solution
canPartitionDP :: [Int] -> Bool
canPartitionDP nums =
  let totalSum = sum nums
      (target, remainder) = totalSum `divMod` 2
      initialDP = Array.listArray (0, target) (True : repeat False) :: Array.Array Int Bool
      finalDP = foldl' updateDP initialDP nums
   in remainder == 0 && (finalDP Array.! target)

updateDP :: Array.Array Int Bool -> Int -> Array.Array Int Bool
updateDP dp num =
  -- For each possible sum i from target down to num:
  -- - Check if we can achieve (i - num) with previous numbers
  -- - If yes, then adding 'num' gives us sum i
  -- - Iterate backwards to avoid using the same number twice
  dp
    Array.// [ (i, True)
             | i <- [target, target - 1 .. num],
               dp Array.! (i - num)
             ]
  where
    target = snd (Array.bounds dp)

-- =============================================================================
-- TESTING FRAMEWORK
-- =============================================================================

data TestCase a b = TestCase
  { name :: String, -- A descriptive name for the test
    input :: a, -- The input to the function
    expected :: b -- The expected output
  }

testCases :: [TestCase [Int] Bool]
testCases =
  [ TestCase "Example 1: [1,5,11,5] -> True" [1, 5, 11, 5] True,
    TestCase "Example 2: [1,2,3,5] -> False" [1, 2, 3, 5] False,
    TestCase "Single element [2] -> False" [2] False,
    TestCase "Two equal elements [4,4] -> True" [4, 4] True,
    TestCase "Empty array [] -> True" [] True,
    TestCase "All zeros [0,0,0,0] -> True" [0, 0, 0, 0] True,
    TestCase "Large equal partition [100,100,100,100] -> True" [100, 100, 100, 100] True,
    TestCase "No equal partition [1,2,5] -> False" [1, 2, 5] False
  ]

-- Helper function to run a single test case for Partition Equal Subset Sum.
-- Takes a partition function and a test case, and runs the test.
runPartitionTest :: ([Int] -> Bool) -> TestCase [Int] Bool -> IO ()
runPartitionTest partitionFunc tc = do
  let nums = input tc
      actual = partitionFunc nums
  if actual == expected tc
    then printf "\x1b[32m[PASS]\x1b[0m %s\n" (name tc) -- Green for PASS
    else do
      printf "\x1b[31m[FAIL]\x1b[0m %s\n" (name tc) -- Red for FAIL
      printf "       Input:    nums=%s\n" (show nums)
      printf "       Expected: %s\n" (show $ expected tc)
      printf "       Actual:   %s\n" (show actual)

main :: IO ()
main = do
  putStrLn "-----------------------------------------"
  putStrLn "Running tests for Partition Equal Subset Sum ..."
  putStrLn "-----------------------------------------"
  putStrLn "Testing canPartition:"
  mapM_ (runPartitionTest canPartitionDP) testCases
  putStrLn "-----------------------------------------"
  putStrLn "Tests complete."
