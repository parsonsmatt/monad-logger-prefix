# [monad-logger-prefix][]

[![Build Status](https://travis-ci.org/parsonsmatt/monad-logger-prefix.svg?branch=master)](https://travis-ci.org/parsonsmatt/monad-logger-prefix)

This package provides an easy way to add prefixes to any
[`MonadLogger`][monad-logger]. Here's a brief example:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.Logger
import Control.Monad.Logger.Prefix

main :: IO ()
main = runStdoutLoggingT $ do
    $(logDebug) "This one has no prefix."

    "foo" `prefixLogs` do
        $(logDebug) "This one has a [foo] prefix."

        "bar" `prefixLogs` do
            $(logDebug) "This one has both [foo] and [bar] prefixes."
```

The package includes a benchmark demonstrating that there is no performance difference with ordinary logging.

[monad-logger-prefix]: https://github.com/githubuser/monad-logger-prefix
[monad-logger]: https://hackage.haskell.org/package/monad-logger
