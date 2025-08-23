-- Leetcode 746. Min Cost Climbing Stairs

module Solution where

import Data.List (foldl')

-- |
-- Correct! But not efficient.
-- `!!` operation is O(n), so in total this function is O(n^2).
minCostClimbingStairsMemo :: [Int] -> Int
minCostClimbingStairsMemo cost = last minCost
  where
    minCost = map calculate [0 .. length cost]
    calculate 0 = 0
    calculate 1 = 0
    calculate x = min (cost !! (x - 2) + minCost !! (x - 2)) (cost !! (x - 1) + minCost !! (x - 1))

-- |
--  BUG:
--  This is a common but incorrect attempt to solve the DP problem using
--  a self-referential infinite list in Haskell.
--
--  *** THE BUG: LAZY EVALUATION DEADLOCK ***
--  This function will hang (never terminate) on most inputs. The reason is that
--  the definition for an element in `minCost` depends on the element itself,
--  creating a dependency cycle that lazy evaluation cannot resolve.
--
--  It's like a circular definition in a spreadsheet, e.g., cell A1 = B1 + C1,
--  and cell B1 = A1 + D1. The calculation can never complete.
minCostClimbingStairs_HangingAttempt :: [Int] -> Int
minCostClimbingStairs_HangingAttempt cost =
  -- This part of the logic is also incorrect. The final answer should be the
  -- minimum of the last two costs calculated, not just the last one.
  minCost !! length cost
  where
    -- This self-referential list is where the main problem lies.
    minCost =
      0
        : 0
        : zipWith
          min
          -- This term represents the cost of taking a two-step jump to the
          -- current position.
          ( zipWith
              (+)
              -- PROBLEM SOURCE: To calculate `minCost !! 2`, this expression
              -- needs to access `minCost !! 2`. `minCost !! 2` is waiting for this
              -- expression to produce a value. This is a deadlock.
              (drop 2 minCost)
              (drop 2 cost)
          )
          -- This term represents the cost of taking a one-step jump to the
          -- current position.
          ( zipWith
              (+)
              (drop 1 minCost)
              (drop 1 cost)
          )

-- | Time: O(n), Space: O(1)
minCostClimbingStairs :: [Int] -> Int
minCostClimbingStairs cost =
  -- The fold computes the DP table iteratively.
  -- The accumulator (a, b) stores the min costs to reach the
  -- previous two steps. We start with (0, 0) for the "floor"
  -- before the first two steps.
  let (finalA, finalB) = foldl' step (0, 0) cost
   in -- The result is the minimum cost to reach one of the last two steps,
      -- as we can take a final step from either to reach the top.
      min finalA finalB
  where
    -- The step function calculates the next state of our DP.
    -- Given (cost_to_reach_i-2, cost_to_reach_i-1) and the
    -- current step's cost, it returns the next pair:
    -- (cost_to_reach_i-1, cost_to_reach_i).
    step :: (Int, Int) -> Int -> (Int, Int)
    step (prev2, prev1) currentCost =
      (prev1, currentCost + min prev2 prev1)

testHelper :: [Int] -> Int -> IO ()
testHelper cost expected = do
  let result = minCostClimbingStairs cost
  putStrLn $ "Input cost: " ++ show cost
  putStrLn $ "Expected: " ++ show expected
  if result == expected
    then putStrLn $ "Test passed: " ++ show result
    else putStrLn $ "Test failed: expected " ++ show expected ++ ", got " ++ show result
  putStrLn ""

main :: IO ()
main = do
  putStrLn "---------- Testing minCostClimbingStairsMemo: ---------\n"
  testHelper [10, 15, 20] 15
  testHelper [1, 100, 1, 1, 1, 100, 1, 1, 100, 1] 6
  putStrLn "-----------------------------------------------------"
