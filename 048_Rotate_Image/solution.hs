import Data.List

rotate90 :: [[a]] -> [[a]]
rotate90 = map reverse . transpose
