{-|
Module      : Control.Monad.Logger.Prefix
Description : Short description
Copyright   : (c) Seller Labs, 2016
License     : Apache 2.0
Maintainer  : matt@sellerlabs.com
Stability   : experimental
Portability : POSIX

This module exports the 'LogPrefixT' monad transfomer. This transformer adds
a given prefix to a 'MonadLogger' context, allowing you to make your logs a bit
more greppable without including much boilerplate. The prefixes can be nested
easily.

The function 'prefixLogs' is the most convenient way to use the library. All you
have to do is use the function to add the prefix, and it Just Works. Here's an
example:

@
someLoggingFunction :: MonadLogger m => m ()
someLoggingFunction = do
    $(logDebug) "No prefix here"
    "foo" \`prefixLogs\` do
        $(logDebug) "There's a [foo] there!
        "bar" \`prefixLogs\` do
            $(logDebug) "Now there's a [foo] *and* a [bar]"
@
-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Logger.Prefix
    ( -- * LogPrefixT
      LogPrefixT()
    , prefixLogs
    ) where

import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.Monoid
import           Data.Text                   (Text)


-- | This function runs the underlying 'MonadLogger' instance with a prefix
-- using the 'LogPrefixT' transformer.
--
-- >>> :set -XOverloadedStrings
-- >>> let l = logDebugN "bar"
-- >>> runStdoutLoggingT (prefixLogs "foo" (logDebugN "bar"))
-- [Debug] [foo] bar
prefixLogs :: MonadLogger m => Text -> LogPrefixT m a -> m a
prefixLogs prefix =
    flip runReaderT (toLogStr $! mconcat ["[", prefix, "] "]) . runLogPrefixT

-- | 'LogPrefixT' is a monad transformer that prepends a bit of text to each
-- logging action in the current 'MonadLogger' context. The internals are
-- currently implemented as a wrapper around 'ReaderT' 'Text'.
newtype LogPrefixT m a = LogPrefixT { runLogPrefixT :: ReaderT LogStr m a }
    deriving
        (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadThrow, MonadCatch)

instance MonadLogger m => MonadLogger (LogPrefixT m) where
    monadLoggerLog loc src lvl msg = LogPrefixT $ ReaderT $ \prefix ->
        monadLoggerLog loc src lvl (toLogStr prefix <> toLogStr msg)

instance MonadBase b m => MonadBase b (LogPrefixT m) where
    liftBase = lift . liftBase

instance MonadBaseControl b m => MonadBaseControl b (LogPrefixT m) where
     type StM (LogPrefixT m) a = StM m a
     liftBaseWith f = LogPrefixT $ ReaderT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ runInBase . (\(LogPrefixT r) -> runReaderT r reader')
     restoreM = LogPrefixT . ReaderT . const . restoreM

instance MonadReader r m => MonadReader r (LogPrefixT m) where
    ask = lift ask
    local = mapLogPrefixT . local

instance MonadState s m => MonadState s (LogPrefixT m) where
    get = lift get
    put = lift . put

-- | Maps a given function over the original
mapLogPrefixT :: (m a -> n b) -> LogPrefixT m a -> LogPrefixT n b
mapLogPrefixT f rfn =
    LogPrefixT . ReaderT $ f . runReaderT (runLogPrefixT rfn)
