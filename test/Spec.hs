{-# LANGUAGE OverloadedStrings #-}
module Spec where
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit
import Test.QuickCheck hiding (property)
import Test.HUnit

import Control.Monad

import Data.String.Builder

shouldBe :: (Show a, Eq a) => a -> a -> Assertion
actual `shouldBe` expected = unless (actual == expected) (assertFailure message)
  where message = show actual ++ " was not equal to " ++ show expected

main = hspec $ do
  describe "build" $ do
    it "can be used to construct multi-line string literals in a monadic way" $ do
      let mystring = build $ do
          "foo"
          "bar"
          "baz"
      mystring `shouldBe` "foo\nbar\nbaz\n"
