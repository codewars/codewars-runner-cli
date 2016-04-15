{-# LANGUAGE PackageImports #-}
module Test.CodeWars.Runner (hspec) where
import System.Environment (withArgs)
import qualified System.IO as IO 
import Test.CodeWars.Formatters (codewars)
import qualified System.Exit
import Control.Monad (unless)
import "hspec" Test.Hspec (Spec)
import "hspec" Test.Hspec.Runner ( hspecWithResult
                                 , defaultConfig
                                 , Config
                                   ( configFormatter
                                   , configFastFail)
                                 , Summary (summaryFailures))

hspec :: Spec -> IO ()
hspec spec = withArgs [] $ do
    r <- hspecWithResult config spec
    unless (summaryFailures r == 0)
      System.Exit.exitFailure
    where
      config =
        defaultConfig { configFormatter = Just codewars
                      , configFastFail = True}
