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

removeNthFromEnd' :: [a] -> Int -> [a]
removeNthFromEnd' [] _ = []
removeNthFromEnd' initialList n
  | n <= 0 = initialList
  | otherwise =
      -- This list is used to check if 'n' is larger than the list length.
      -- If we can drop (n-1) elements and the result is empty, 'n' is too large.
      let boundaryCheckList = drop (n - 1) initialList
       in if null boundaryCheckList
            then initialList
            else traverseAndRemove initialList (drop 1 boundaryCheckList)
  where
    -- Helper function that uses two list "pointers" to find the correct node.
    traverseAndRemove :: [a] -> [a] -> [a]
    traverseAndRemove [] _ = []
    -- Base Case: The fast pointer is empty, so the slow pointer's head
    -- is the element to remove. We return the slow pointer's tail.
    traverseAndRemove (_ : slowTail) [] = slowTail
    -- Recursive Step: Prepend the slow pointer's head to the result of
    -- traversing the rest of both lists. This rebuilds the front of the list.
    traverseAndRemove (head : slowTail) (_ : fastTail) = head : traverseAndRemove slowTail fastTail

-- Should not be reached with this logic, but makes the function total.

-- Helper function to print test case results neatly.
printTest :: (Show a, Eq a) => String -> [a] -> Int -> [a] -> IO ()
printTest caseName inputList n expected = do
  let actual = removeNthFromEnd' inputList n
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
