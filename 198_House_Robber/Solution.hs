-- Leetcode 198. House Robber

module Solution where

import Data.List (foldl')
import Text.ParserCombinators.ReadPrec (step)

rob :: [Int] -> Int
rob value = snd $ foldl' step (0, 0) value
  where
    step (prev1, prev2) currentValue = (prev2, max (prev1 + currentValue) prev2)

-- | Correct! But not efficient.
-- `!!` operation is O(n), so in total this function is O(n^2).
robMemo :: [Int] -> Int
robMemo [] = 0
robMemo [singleHouse] = singleHouse
robMemo houses@(firstHouse : secondHouse : _) = last maxRobAtEachHouse
  where
    numHouses = length houses
    maxRobAtEachHouse = map calculateMaxRobAtIndex [0 .. (numHouses - 1)]

    calculateMaxRobAtIndex :: Int -> Int
    calculateMaxRobAtIndex 0 = firstHouse
    calculateMaxRobAtIndex 1 = max firstHouse secondHouse
    calculateMaxRobAtIndex currentIndex =
      let dontRobCurrent = maxRobAtEachHouse !! (currentIndex - 1)
          robCurrent = houses !! currentIndex + maxRobAtEachHouse !! (currentIndex - 2)
       in max dontRobCurrent robCurrent

testHelper :: [Int] -> Int -> IO ()
testHelper input expected =
  if result == expected
    then putStrLn $ "Test passed: " ++ show result
    else putStrLn $ "Test failed: got " ++ show result ++ ", expected " ++ show expected
  where
    result = robMemo input

main :: IO ()
main = do
  putStrLn "---------- Testing robMemo: ---------\n"
  testHelper [1, 2, 3, 1] 4
  testHelper [2, 7, 9, 3, 1] 12
  testHelper [2, 1, 1, 2] 4
  testHelper [1, 3, 1] 3
  testHelper [1, 2] 2
  testHelper [2] 2
  testHelper [] 0
