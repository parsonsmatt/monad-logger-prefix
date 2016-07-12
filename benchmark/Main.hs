{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (log)

import Control.Monad
import Criterion.Main

import Control.Monad.Logger
import Control.Monad.Logger.Prefix

main :: IO ()
main = defaultMain
    [ bgroup "no prefix"
        [ bench "1: " $ whnfIO $ runStdoutLoggingT log
        , bench "10: " $ whnfIO $ runStdoutLoggingT (10 `times` log)
        , bench "100: " $ whnfIO $ runStdoutLoggingT (100 `times` log)
        ]
    , bgroup "one prefix"
        [ bench "1: " $ whnfIO $ runStdoutLoggingT (pref log)
        , bench "10: " $ whnfIO $ runStdoutLoggingT (10 `times` pref log)
        , bench "100: " $ whnfIO $ runStdoutLoggingT (100 `times` pref log)
        ]
    , bgroup "two prefixes"
        [ bench "1: " $ whnfIO $ runStdoutLoggingT (pref (pref log))
        , bench "10: " $ whnfIO $ runStdoutLoggingT (10 `times` pref (pref log))
        , bench "100: " $ whnfIO $ runStdoutLoggingT (100 `times` pref (pref log))
        ]
    ]

pref :: MonadLogger m => LogPrefixT m a -> m a
pref = prefixLogs "foobar"

log :: MonadLogger m => m ()
log = logDebugN "hello world"

times :: Monad m => Int -> m a -> m ()
times i act = forM_ [1..i] (const act)
