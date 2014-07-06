{-# LANGUAGE NoMonomorphismRestriction #-}
module Test.CodeWars (
  -- CodeWars Specification system 
  test

  -- HSpec
  , Spec
  , describe
  , it

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
                     ) where

import Test.Hspec 
import Test.CodeWars.Runner (test)
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
