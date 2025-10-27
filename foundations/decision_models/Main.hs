-- Add this language extension pragma at the very top of the file
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Our pure logic module
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import DecisionModel

-- IO Actions (These remain the same, they are our side-effects)
-- =============================================================

sendHerIntoTheHouse :: IO ()
sendHerIntoTheHouse = TIO.putStrLn ">> IO side-effect: Executed high-risk action."

askForMoreInfo :: IO ()
askForMoreInfo = TIO.putStrLn ">> IO side-effect: Executed info-gathering action."

abortThePlan :: IO ()
abortThePlan = TIO.putStrLn ">> IO side-effect: Executed abort action."

-- New Reporting Function
-- ======================
-- This function now handles the presentation logic.
runAndReportSimulation :: Text -> MyState -> AvailableInfo -> IO ()
runAndReportSimulation scenarioName myState info = do
  TIO.putStrLn $ "--- " <> scenarioName <> " ---"

  -- 1. Get the pure decision and rationale
  let (maybeAction, rationale) = decideAndActPure myState info

  -- 2. Print the analysis behind the decision
  TIO.putStrLn $ "🧠 Analysis: " <> rationale

  -- 3. Determine and report the final, clear action
  TIO.putStrLn "⚡️ Action:   "
  case maybeAction of
    Just Proceed -> do
      TIO.putStrLn "Proceed with the original plan."
      sendHerIntoTheHouse -- The IO action is still here
    Just GatherMoreData -> do
      TIO.putStrLn "Execute: Ask clarifying questions to reduce risk."
      askForMoreInfo -- The IO action is still here
    Just Abort -> do
      TIO.putStrLn "Execute: Abort the plan immediately."
      abortThePlan -- The IO action is still here
    Nothing -> do
      TIO.putStrLn "Pause and reassess. No external action taken."
  -- No world-altering IO action is called here.

  TIO.putStrLn "-----------------------------------------------------\n"

-- Main Entry Point
-- =================
-- Main is now much cleaner, just setting up scenarios.
main :: IO ()
main = do
  -- Define our common info object
  let dreamInfo =
        AvailableInfo
          { pastExperience = ["國中同學", "家庭單純溫和", "第一次聊天很愉快"],
            currentData = ["她主動請求搭便車"],
            knownRisks = []
          }

  -- Run Simulation 1
  runAndReportSimulation
    "Scenario: Angry & Distracted State"
    (Unstable "Angry and distracted")
    dreamInfo

  -- Run Simulation 2
  runAndReportSimulation
    "Scenario: Stable State, but Insufficient Info"
    Stable
    (dreamInfo {currentData = []}) -- Simulate not having current data
