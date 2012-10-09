{-# LANGUAGE GADTs #-}
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

-- a writer monad
data BuilderM a = BuilderM a ShowS

instance Monad BuilderM where
  return a            = BuilderM a id
  BuilderM a xs >>= f = case f a of
    BuilderM b ys -> BuilderM b (xs . ys)

type Builder = BuilderM ()

literal :: String -> Builder
literal = BuilderM () . showString

instance (a ~ ()) => IsString (BuilderM a) where
  fromString s = literal s >> literal "\n"

build :: Builder -> String
build (BuilderM () s) = s ""
