{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main (main, spec) where

import           Test.Hspec.ShouldBe

import           Data.String
import           Data.String.Builder

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "build" $ do
    it "can be used to construct multi-line string literals in a monadic way" $ do
      build $ do
        "foo"
        "bar"
        "baz"
      `shouldBe` "foo\nbar\nbaz\n"

    prop "works for arbitrary input" $ do
      \l -> (build . sequence_ . map fromString) l == unlines l
