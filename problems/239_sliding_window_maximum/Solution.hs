-- Leetcode 239. Sliding Window Maximum
-- NOTE: I don't understand how this is used, this is just paste here

module Solution where

import Data.Foldable (toList)
import Data.Sequence (Seq (..), (<|), (|>))
import qualified Data.Sequence as Seq

-- The `nums` array is passed implicitly via the function's closure.
-- This is a common pattern to avoid passing a large, unchanging structure at every step.
maxSlidingWindow :: [Int] -> Int -> [Int]
maxSlidingWindow nums k =
  let -- The "step" function for our fold.
      -- It takes the state (queue, results) and the next element (index, value).
      -- It returns the new state.
      step (queue, results) (idx, val) =
        let -- 1. Filter outdated indices from the front.
            queue1 = Seq.dropWhileL (\i -> i <= idx - k) queue

            -- 2. Filter indices of smaller values from the back.
            queue2 = Seq.dropWhileR (\i -> (nums !! i) < val) queue1

            -- 3. Add the new index to the back.
            queue3 = queue2 |> idx

            -- 4. If the window is full, prepend the max to our results.
            newResults =
              if idx >= k - 1
                -- The max is at the front of the queue.
                then (nums !! Seq.index queue3 0) : results
                else results
         in (queue3, newResults)

      -- The list of (index, value) pairs to fold over.
      indexedNums = zip [0 ..] nums

      -- The initial state for the fold: an empty queue and an empty result list.
      initialState = (Seq.empty, [])

      -- Execute the fold and get the final state.
      finalState = foldl step initialState indexedNums

      -- The result list is the second part of the final state, reversed.
      (_, finalResultsReversed) = finalState
   in reverse finalResultsReversed
