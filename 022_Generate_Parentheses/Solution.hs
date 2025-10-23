-- Leetcode 22. Generate Parentheses
module Solution where

generateParenthesis :: Int -> [String]
generateParenthesis = go "" 0 0
  where
    go :: String -> Int -> Int -> Int -> [String]
    go currentStr openCount closeCount maxPairs
      | length currentStr == maxPairs * 2 && openCount == closeCount = [currentStr]
      | otherwise =
          let withOpen =
                if openCount < maxPairs
                  then go (currentStr ++ "(") (openCount + 1) closeCount maxPairs
                  else []
              withClose =
                if closeCount < openCount
                  then go (currentStr ++ ")") openCount (closeCount + 1) maxPairs
                  else []
           in withOpen ++ withClose
