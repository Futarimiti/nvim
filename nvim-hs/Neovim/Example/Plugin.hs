{-# LANGUAGE TemplateHaskell #-}
-- Template Haskell is used to remove a lot of manual boiler-plate from
-- declaring the functions you want to export.
module Neovim.Example.Plugin
  ( plugin
  ) where

import           Neovim

import           Neovim.Example.Plugin.Fibonacci (fibonacci)
import           Neovim.Example.Plugin.Random    (nextRandom, randomNumbers,
                                                  setNextRandom)

plugin :: Neovim () NeovimPlugin
plugin = do
    randomPluginState <- randomNumbers
    wrapPlugin Plugin
        { environment = randomPluginState
        , exports =
            [ $(function' 'fibonacci) Sync
            -- Notice the quotation mark before the functin name, this is important!
            , $(function' 'nextRandom) Sync
            , $(function "SetNextRandom" 'setNextRandom) Async
            ]
        }
