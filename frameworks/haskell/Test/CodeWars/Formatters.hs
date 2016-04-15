module Test.CodeWars.Formatters (codewars) where
import Test.Hspec.Formatters ( Formatter (..)
                             , FailureRecord (..)
                             , FormatM
                             , specdoc
                             , newParagraph
                             , getRealTime
                             , getFailMessages
                             , formatException)
import qualified Test.Hspec.Formatters (writeLine)
import Control.Monad (unless, join, forM_)
import Text.Printf (printf)
import Data.List (intercalate)
import Data.List.Split (splitOn)

writeLine :: String -> FormatM ()
writeLine s = Test.Hspec.Formatters.writeLine
              $ intercalate "<:LF:>"
              $ splitOn "\n" s

codewars :: Formatter
codewars = do
specdoc {
  headerFormatter = return ()
, exampleGroupStarted = \nesting name -> do
    -- separate groups with an empty line
    -- TODO: Handel nesting
    writeLine $ join $ ["<DESCRIBE::>", name]

, exampleGroupDone = Test.Hspec.Formatters.writeLine "<COMPLETEDIN::>"

, exampleSucceeded = \(_, requirement) -> do
    writeLine $ join ["<IT::>", requirement]
    writeLine "<PASSED::>Test Passed"

, exampleFailed = \(_, requirement) _ -> do
    writeLine $ join ["<IT::>" , requirement]

, failedFormatter = do
    failures <- getFailMessages
    forM_ failures formatFailure

, footerFormatter = do
    time <- getRealTime
    writeLine $ printf "<COMPLETEDIN::>%1.4f seconds" time
} where
   formatFailure :: FailureRecord -> FormatM ()
   formatFailure (FailureRecord {failureRecordMessage = reason}) =
     unless (null err) $ writeLine err
     where
       err = either
              ((printf "<ERROR::>%s") . formatException)
              (printf "<FAILED::>%s")
              reason
