module Test.CodeWars.Formatters (codewars) where
import Test.Hspec.Formatters ( Formatter (..)
                             , specdoc
                             , writeLine
                             , newParagraph
                             , getRealTime
                             , getCPUTime)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Control.Monad (unless, join)
import Text.Printf (printf)

codewars :: Formatter
codewars = do
specdoc {
  headerFormatter = return (),
  exampleGroupStarted = \n nesting name -> do
   -- separate groups with an empty line
    unless (n == 0) newParagraph
    writeLine $ join [indentationFor nesting,
                      "<DESCRIBE::>",
                      name]

, exampleGroupDone = newParagraph

, exampleSucceeded = \(nesting, requirement) -> do
    writeLine $ join [indentationFor nesting
                     , "<PASSED::> - "
                     , requirement]

, exampleFailed = \(nesting, requirement) _ -> do
    writeLine $ join [indentationFor nesting
                     , "<ERROR::> - "
                     , requirement]
, footerFormatter =   
    writeLine =<< (++)
      <$> (printf "Finished in %1.4f seconds"
      <$> getRealTime) <*> (maybe "" (printf ", used %1.4f seconds of CPU time") <$> getCPUTime)
} where
    indentationFor nesting = replicate (length nesting * 2) ' '
