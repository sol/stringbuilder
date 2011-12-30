{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-missing-signatures #-}
module Spec where

import Test.Hspec.ShouldBe

import Data.String.Builder

main = hspec $ do
  describe "build" $ do
    it "can be used to construct multi-line string literals in a monadic way" $ do
      build $ do
        "foo"
        "bar"
        "baz"
      `shouldBe` "foo\nbar\nbaz\n"
