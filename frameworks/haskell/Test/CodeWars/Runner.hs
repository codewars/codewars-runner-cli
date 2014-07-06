module Test.CodeWars.Runner (test) where
import System.Environment (withArgs)
import qualified System.IO as IO 
import Test.CodeWars.Formatters (codewars)
import qualified System.Exit
import Control.Monad (unless)
import Test.Hspec (Spec)
import Test.Hspec.Runner ( hspecWith
                         , defaultConfig
                         , Config
                           ( configFormatter
                           , configFastFail)
                         , Summary (summaryFailures))

test :: Spec -> IO ()
test spec = withArgs [] $ do
    r <- hspecWith config spec
    unless (summaryFailures r == 0)
      System.Exit.exitFailure
    where
      config =
        defaultConfig { configFormatter = codewars
                      , configFastFail = True}
