-- Leetcode 13. Roman to Integer

import qualified Data.Map as Map

romanToInt :: String -> Int
romanToInt [] = 0
romanToInt [c] = lookupVal c
romanToInt (c1 : c2 : cs)
  | v1 < v2 = (v2 - v1) + romanToInt cs
  | otherwise = v1 + romanToInt (c2 : cs)
  where
    v1 = lookupVal c1
    v2 = lookupVal c2

lookupVal :: Char -> Int
lookupVal c = Map.findWithDefault 0 c romanMap

romanMap :: Map.Map Char Int
romanMap =
  Map.fromList
    [ ('I', 1),
      ('V', 5),
      ('X', 10),
      ('L', 50),
      ('C', 100),
      ('D', 500),
      ('M', 1000)
    ]

main :: IO ()
main = mapM_ printVal testcases
  where
    printVal s = putStrLn $ s ++ " -> " ++ show (romanToInt s)
    testcases = ["III", "LVIII", "MCMXCIV", "MMMCMXCIX"]
