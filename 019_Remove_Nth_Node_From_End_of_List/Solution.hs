-- Leetcode 19. Remove Nth Node From End of List
--
-- Takeaway: `splitAt` and `drop`

module Solution where

removeNthFromEnd :: [a] -> Int -> [a]
removeNthFromEnd [] n = []
removeNthFromEnd lst n
  | n < 0 || n > length lst = lst
  | otherwise =
      let len = length lst
          -- splitAt breaks the list at this index.
          -- For a list of 5 and n=2, index is 3.
          -- splitAt 3 [1,2,3,4,5] -> ([1,2,3], [4,5])
          (front, back) = splitAt (len - n) lst
       in -- Don't use tail here. Tail is a partial function, it throws an
          -- error on empty lists
          front ++ drop 1 back

-- Helper function to print test case results neatly.
printTest :: (Show a, Eq a) => String -> [a] -> Int -> [a] -> IO ()
printTest caseName inputList n expected = do
  let actual = removeNthFromEnd inputList n
  putStrLn $ "--- " ++ caseName ++ " ---"
  putStrLn $ "Input:  list = " ++ show inputList ++ ", n = " ++ show n
  putStrLn $ "Output: " ++ show actual
  putStrLn $ "Result: " ++ if actual == expected then "✅ Passed" else "❌ Failed"
  putStrLn ""

-- Main function to run all test cases.
main :: IO ()
main = do
  putStrLn "--- Testing removeNthFromEnd ---"

  printTest "Standard Case" [1, 2, 3, 4, 5] 2 [1, 2, 3, 5]
  printTest "Remove First Element" [1, 2, 3, 4, 5] 5 [2, 3, 4, 5]
  printTest "Remove Last Element" [1, 2, 3, 4, 5] 1 [1, 2, 3, 4]
  printTest "Single Element List" [1] 1 []
  printTest "Invalid n (too large)" [1, 2, 3] 4 [1, 2, 3]
  printTest "Invalid n (zero)" [1, 2, 3] 0 [1, 2, 3]
  printTest "Empty List" ([] :: [Int]) 2 []

  putStrLn "--- End of Tests ---"
