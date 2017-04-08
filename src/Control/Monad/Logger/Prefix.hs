{-|
Module      : Control.Monad.Logger.Prefix
Description : Convenient scope-based prefixes for log output
Copyright   : (c) Seller Labs, 2016-2017
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
someLoggingFunction :: 'MonadLogger' m => 'LogPrefixT' m ()
someLoggingFunction = do
    'logDebugN' "No prefix here"
    'prefixLogs' "foo" $ do
        'logDebugN' "There's a [foo] there!
        "bar" \`prefixLogs\` do
            'logDebugN' "Now there's a [foo] *and* a [bar]"
            'logDebugN' "The backticks save you a dollar."
@
-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Logger.Prefix
    ( -- * LogPrefixT
      prefixLogs
    , runLogPrefixT
    , LogPrefixT()
      -- * Reexports
    , module Control.Monad.Logger
    ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import           Data.Text                    (Text)

import           Prelude

-- $setup
-- >>> :set -XOverloadedStrings

-- | This function runs the underlying 'MonadLogger' instance with a prefix
-- using the 'LogPrefixT' transformer.
--
-- >>> let runLog = runStdoutLoggingT . runLogPrefixT
-- >>> let log = logDebugN
-- >>> runLog $ prefixLogs "foo" (log "bar")
-- [Debug] [foo] bar
-- >>> runLog $ prefixLogs "foo" $ prefixLogs "bar" $ log "asdf"
-- [Debug] [foo] [bar] asdf
prefixLogs :: Monad m => Text -> LogPrefixT m a -> LogPrefixT m a
prefixLogs prefix (LogPrefixT action) =
    LogPrefixT (local (<> prefix') action)
  where
    prefix' = toLogStr $! mconcat ["[", prefix, "] "]
{-# INLINE prefixLogs #-}

infixr 5 `prefixLogs`

-- | 'LogPrefixT' is a monad transformer that prepends a bit of text to each
-- logging action in the current 'MonadLogger' context. The internals are
-- currently implemented as a wrapper around 'ReaderT' 'LogStr'. To append
-- another prefix to the chain, use 'prefixLogs'.
newtype LogPrefixT m a = LogPrefixT { unLogPrefixT :: ReaderT LogStr m a }
    deriving
        (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadThrow, MonadCatch, MonadMask)

-- | Unwrap the log prefix constructor, returning the chosen base.
runLogPrefixT :: LogPrefixT m a -> m a
runLogPrefixT (LogPrefixT ma) = runReaderT ma ""

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

instance MonadError e m => MonadError e (LogPrefixT m) where
    throwError = lift . throwError
    catchError err k = LogPrefixT
        $ ReaderT
        $ \prfx -> runReaderT (unLogPrefixT err) prfx
            `catchError`
                \e -> runReaderT (unLogPrefixT (k e)) prfx

instance MonadWriter w m => MonadWriter w (LogPrefixT m) where
    tell = lift . tell
    listen = mapLogPrefixT listen
    pass = mapLogPrefixT pass

instance MonadResource m => MonadResource (LogPrefixT m) where
    liftResourceT = lift . liftResourceT

mapLogPrefixT :: (m a -> n b) -> LogPrefixT m a -> LogPrefixT n b
mapLogPrefixT f rfn =
    LogPrefixT . ReaderT $ f . runReaderT (unLogPrefixT rfn)
