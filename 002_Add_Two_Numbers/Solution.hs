-- Leetcode 2. Add Two Numbers

import LinkedList

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
