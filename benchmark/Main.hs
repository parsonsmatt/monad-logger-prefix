module Main where

import Criterion.Main

import Control.Monad.Logger
import Control.Monad.Logger.Prefix

main :: IO ()
main = defaultMain
    [ bench "const" (whnf const ())
    ]
