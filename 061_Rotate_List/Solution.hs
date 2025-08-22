-- Leetcode 61. Rotate List

module Solution where

rotateRight :: [a] -> Int -> [a]
rotateRight [] _ = []
rotateRight lst k =
  let lstLen = length lst
      rotateOffset = lstLen - k `mod` lstLen
      (front, back) = splitAt rotateOffset lst
   in back ++ front


main :: IO ()
main = do
    putStrLn "Test Case 1:"
    let input1 = [1,2,3,4,5]
    let k1 = 2
    let expected1 = [4,5,1,2,3]
    let result1 = rotateRight input1 k1
    putStrLn $ "Input: list = " ++ show input1 ++ ", k = " ++ show k1
    putStrLn $ "Expected: " ++ show expected1
    putStrLn $ "Got:      " ++ show result1
    putStrLn $ "Pass:     " ++ show (result1 == expected1)
    putStrLn ""

    putStrLn "Test Case 2:"
    let input2 = [0,1,2]
    let k2 = 4
    let expected2 = [2,0,1]
    let result2 = rotateRight input2 k2
    putStrLn $ "Input: list = " ++ show input2 ++ ", k = " ++ show k2
    putStrLn $ "Expected: " ++ show expected2
    putStrLn $ "Got:      " ++ show result2
    putStrLn $ "Pass:     " ++ show (result2 == expected2)
