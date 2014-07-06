module Test.CodeWars.Formatters (codewars) where
import Test.Hspec.Formatters ( Formatter (..)
                             , FailureRecord (..)
                             , FormatM
                             , specdoc
                             , write
                             , writeLine
                             , newParagraph
                             , getRealTime
                             , getFailMessages
                             , formatException)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Control.Monad (unless, join, forM_)
import Text.Printf (printf)

codewars :: Formatter
codewars = do
specdoc {
  headerFormatter = return (),
  exampleGroupStarted = \n _ name -> do
   -- separate groups with an empty line
    unless (n == 0) newParagraph
    writeLine $ join ["<DESCRIBE::>",name]

, exampleGroupDone = newParagraph

, exampleSucceeded = \(_, requirement) -> do
    writeLine $ join ["<IT::>", requirement]
    write "<PASSED::>Test Passed"

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
   formatFailure (FailureRecord _ reason) = 
     unless (null err) $ writeLine err
     where
       err = either
              (("<ERROR::>" ++) . formatException)
              (("<FAILED::>" ++) . (filter (/= '\n')))
              reason


--formatFailure (FailureRecord path reason) =
