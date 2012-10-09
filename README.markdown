# About

`build` can be used to construct multi-line string literals in a monadic way.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.String.Builder

str :: String
str = build $ do
  "foo"
  "bar"
  "baz"
```

This is primarily meant for test suites that use a lot of multi-line string
literals, and when it really matters that the syntax is as easy on the eyes as
it can get.

If you need just a few multi-line string literals in production code consider
one of the following options first.

### Haskell's support for multi-line string literals

```haskell
help = "some\n\
       \multi-line\n\
       \help\n\
       \message\n"
```

### Using unlines to construct multi-line string literals

```haskell
help = unlines [
    "some"
  , "multi-line"
  , "help"
  , "message"
  ]
```
