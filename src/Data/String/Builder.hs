{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- `build` can be used to construct multi-line string literals in a monadic
-- way.
--
-- Here is an example:
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
module Data.String.Builder (build, Builder) where

import Prelude hiding (undefined)
import Data.String
import Control.Monad.Trans.Writer

undefined :: a
undefined = error "Data.String.Builder.undefined"

newtype BuilderM a = BuilderM { runBuilderM :: Writer String a }
  deriving Monad

type Builder = BuilderM ()

literal :: String -> Builder
literal = BuilderM . tell

instance IsString (BuilderM a) where
  fromString s = literal s >> literal "\n" >> return undefined

build :: Builder -> String
build = snd . runWriter . runBuilderM
