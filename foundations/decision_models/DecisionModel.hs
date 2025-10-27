{-# LANGUAGE OverloadedStrings #-}
{-
    ============================================================================
    |                                                                          |
    |                      MindOS v0.1: The Decision Model                     |
    |                     Developer's Guide to Core Emotions                   |
    |                                                                          |
    ============================================================================

    This module is more than just code; it's a functional model for navigating
    the complex, often-unstable runtime of the human mind. The types and
    functions below provide a robust protocol for handling difficult internal
    states.
-}

module DecisionModel where

import Data.Text (Text) -- 使用 Text 來處理文字會比 String 更有效率

-- 描述我們自身狀態的類型
data MyState = Stable | Unstable Text deriving (Show, Eq)

-- 決策時所掌握的資訊
data AvailableInfo = AvailableInfo
  { pastExperience :: [Text],
    currentData :: [Text],
    knownRisks :: [Text]
  }
  deriving (Show, Eq)

-- 決策後可能採取的行動
data Action = Proceed | GatherMoreData | Abort deriving (Show, Eq)

-- 解釋決策原因的文字
type Rationale = Text

-- | 根據自身狀態和外部資訊，做出決策。
-- | 這是純函式，沒有任何副作用。
decideAndActPure :: MyState -> AvailableInfo -> (Maybe Action, Rationale)
decideAndActPure myState info
  -- 規則 1: 如果自身系統狀態不穩定，暫停決策 (回傳 Nothing)
  | isUnstable myState =
      ( Nothing,
        "Decision paused. Reason: System state compromised (e.g., angry, tired)."
      )
  -- 規則 2: 如果已知風險太高，中止行動
  | not (null (knownRisks info)) =
      ( Just Abort,
        "Decision: Abort. Reason: Known risks detected."
      )
  -- 規則 3: 如果資訊不足以判斷安全，預設行動是「收集更多資訊」
  -- 這裡我們用一個簡單的邏輯：如果沒有近期資料，就視為資訊不足
  | isInfoInsufficient info =
      ( Just GatherMoreData,
        "Decision: Gather More Data. Reason: Information is incomplete for a high-stakes decision."
      )
  -- 規則 4: 如果以上條件都通過，才繼續
  | otherwise =
      ( Just Proceed,
        "Decision: Proceed. Reason: No known risks and info appears sufficient."
      )

-- 輔助函式，同樣是純函式
isUnstable :: MyState -> Bool
isUnstable (Unstable _) = True
isUnstable Stable = False

isInfoInsufficient :: AvailableInfo -> Bool
isInfoInsufficient info = null (currentData info)
