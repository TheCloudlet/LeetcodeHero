-- Leetcode 2. Add Two Numbers

import LinkedList

-- |
--  Add two numbers represented as linked lists.
--
--  Each linked list represents a non-negative integer where digits are stored
--  in reverse order (least significant digit first). This function adds the
--  two numbers and returns the sum as a new linked list in the same format.
--
--  Algorithm:
--  - Traverse both linked lists simultaneously from head to tail
--  - Add corresponding digits along with any carry from the previous addition
--  - Handle cases where lists have different lengths by treating missing digits
--    as 0
--  - Propagate carry to the next position, creating a new digit if needed
--
--  Time Complexity: O(max(m, n)) where m, n are input list lengths
--  Space Complexity: O(max(m, n)) for the result list
--
--  Examples:
--  >>> addTwoNumbers (fromStdList [2,4,3]) (fromStdList [5,6,4])
--  fromStdList [7,0,8]  -- represents 342 + 465 = 807
--
--  >>> addTwoNumbers (fromStdList [0]) (fromStdList [0])
--  fromStdList [0]      -- represents 0 + 0 = 0
--
--  >>> addTwoNumbers (fromStdList [9,9,9]) (fromStdList [9,9,9,9])
--  fromStdList [8,9,9,0,1]  -- represents 999 + 9999 = 10998
--
--  @param l1 First number as a linked list (digits in reverse order)
--  @param l2 Second number as a linked list (digits in reverse order)
--  @return Sum of the two numbers as a linked list (digits in reverse order)
addTwoNumbers :: LinkedList Int -> LinkedList Int -> LinkedList Int
addTwoNumbers l1 l2 = go l1 l2 0
  where
    go :: LinkedList Int -> LinkedList Int -> Int -> LinkedList Int
    go LNil LNil 0 = LNil
    go LNil LNil carry = LCons carry LNil
    go (LCons x xs) LNil carry = go (LCons x xs) (LCons 0 LNil) carry
    go LNil (LCons y ys) carry = go (LCons 0 LNil) (LCons y ys) carry
    go (LCons x xs) (LCons y ys) carry =
      let total = x + y + carry
          digit = total `mod` 10
          newCarry = total `div` 10
       in LCons digit (go xs ys newCarry)

main :: IO ()
main = do
  let l1 = fromStdList ([2, 3, 6] :: [Int])
  let l2 = fromStdList ([5, 9, 7] :: [Int])
  print $ toStdList $ addTwoNumbers l1 l2
