-- Leetcode 24. Swap Nodes in Pairs

data ListNode = Nil | Node Int ListNode deriving (Show)

swapPairs :: ListNode -> ListNode
swapPairs Nil = Nil
swapPairs (Node x Nil) = Node x Nil -- only one node left
swapPairs (Node x1 (Node x2 rest)) = Node x2 (Node x1 (swapPairs rest))

-- Helper functions

-- Convert regular list to ListNode
fromList :: [Int] -> ListNode
fromList = foldr Node Nil

-- Convert ListNode back to regular list
toList :: ListNode -> [Int]
toList Nil = []
toList (Node x rest) = x : toList rest

-- Try it
main :: IO ()
main =
  mapM_
    print
    [ toList $ swapPairs $ fromList [],
      toList $ swapPairs $ fromList [1],
      toList $ swapPairs $ fromList [1, 2],
      toList $ swapPairs $ fromList [1, 2, 3],
      toList $ swapPairs $ fromList [1, 2, 3, 4],
      toList $ swapPairs $ fromList [10, 20, 30, 40, 50],
      toList $ swapPairs $ fromList [9, 8, 7, 6, 5, 4]
    ]
