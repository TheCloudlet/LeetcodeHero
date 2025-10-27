-- Leetcode 322. Coin Change

module Solution where

import Data.Array
import Data.Maybe

coinChange :: [Int] -> Int -> Int
coinChange coins amount =
  let memo = array (0, amount) [(i, solve i) | i <- [0 .. amount]]

      solve 0 = Just 0
      solve amt
        | amt < 0 = Nothing -- Impossible
        | otherwise =
            let candidates =
                  [ fmap (+ 1) (memo ! (amt - coin))
                  | coin <- coins,
                    amt - coin >= 0
                  ]
                validCandidates = catMaybes candidates
             in if null validCandidates
                  then Nothing
                  else Just (minimum validCandidates)
   in fromMaybe (-1) (memo ! amount)

-- Test helper function
testCoinChange :: [Int] -> Int -> Int -> String
testCoinChange coins amount expected =
  let result = coinChange coins amount
   in if result == expected
        then "✓ PASS: coinChange " ++ show coins ++ " " ++ show amount ++ " = " ++ show result
        else "✗ FAIL: coinChange " ++ show coins ++ " " ++ show amount ++ " = " ++ show result ++ " (expected " ++ show expected ++ ")"

-- Main function with test cases
main :: IO ()
main = do
  putStrLn "--------------------------------------"
  putStrLn "Testing Coin Change Solution"
  putStrLn "--------------------------------------"

  -- Test case 1: coins = [1,3,4], amount = 6 -> 3 (1+1+4 or 3+3)
  putStrLn $ testCoinChange [1, 3, 4] 6 2

  -- Test case 2: coins = [2], amount = 3 -> -1 (impossible)
  putStrLn $ testCoinChange [2] 3 (-1)

  -- Test case 3: coins = [1], amount = 0 -> 0 (no coins needed)
  putStrLn $ testCoinChange [1] 0 0

  -- Test case 4: coins = [1,2,5], amount = 11 -> 3 (5+5+1)
  putStrLn $ testCoinChange [1, 2, 5] 11 3

  -- Test case 5: coins = [2,3,5], amount = 9 -> 3 (3+3+3)
  putStrLn $ testCoinChange [2, 3, 5] 9 3

  -- Test case 6: coins = [1,2,5], amount = 7 -> 2 (5+2)
  putStrLn $ testCoinChange [1, 2, 5] 7 2

  -- Test case 7: Large amount test
  putStrLn $ testCoinChange [1, 3, 4] 15 4

  putStrLn ""
  putStrLn "All tests completed!"
