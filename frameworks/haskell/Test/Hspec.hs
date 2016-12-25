{-# LANGUAGE NoMonomorphismRestriction, PackageImports #-}

-- By using Package Qualified Imports, we can shadow HSpec with our own system
module Test.Hspec (
  -- CodeWars Specification system (shadows Hspec)
  hspec

  -- HSpec
  , Spec
  , describe
  , it
  , context
  , example
  , pending
  , pendingWith
  , before
  , after
  , after_
  , around
  , parallel

  -- QuickCheck
  , property
  -- Exceptions
  , evaluate

  -- Expectation Combinators,
  -- see https://github.com/sol/hspec-expectations/blob/master/src/Test/Hspec/Expectations.hs
  , Expectation
  , expectationFailure
  , shouldBe
  , shouldNotBe
  , shouldSatisfy
  , shouldContain
  , shouldMatchList
  , shouldReturn
  , shouldThrow
  , Selector
  , anyException
  , anyErrorCall
  , anyIOException
  , anyArithException
  , errorCall

  -- BlackList import declarations
  , Hidden (..)
  , BlackList
  , hidden
                     ) where

import "hspec" Test.Hspec hiding (hspec, shouldNotBe)
import Test.CodeWars.Runner (hspec)
import Test.CodeWars.BlackList (Hidden (..), BlackList, hidden)
import Test.QuickCheck hiding (reason)
import Control.Exception (evaluate)
import Test.HUnit (assertFailure)
import Control.Monad (unless)

infix 1 `shouldNotBe`
shouldNotBe :: (Eq a, Show a) => a -> a -> Expectation
shouldNotBe expected actual =
  unless (actual /= expected) (assertFailure msg)
  where msg = "expected: " ++ show expected
              ++ "\n to be different than: " ++ show actual
