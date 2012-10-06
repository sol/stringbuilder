{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}
-- |
-- `build` can be used to construct multi-line string literals in a monadic
-- way.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Data.String.Builder
-- >
-- > mystring :: String
-- > mystring = build $ do
-- >   "foo"
-- >   "bar"
-- >   "baz"
--
-- `return` and `>>=` are not useful in this context!
module Data.String.Builder (build, Builder, BuilderM) where

import Data.String
import Control.Monad.Trans.Writer

newtype BuilderM a = BuilderM {runBuilderM :: Writer String a}
  deriving Monad

type Builder = BuilderM ()

literal :: String -> Builder
literal = BuilderM . tell

instance (a ~ ()) => IsString (BuilderM a) where
  fromString s = literal s >> literal "\n" >> return ()

build :: Builder -> String
build = execWriter . runBuilderM
