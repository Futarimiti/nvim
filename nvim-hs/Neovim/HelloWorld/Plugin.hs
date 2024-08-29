module Neovim.HelloWorld.Plugin (plugin) where

import           Neovim
import           Neovim.HelloWorld.Plugin.Core (helloWorld)

plugin :: Neovim () NeovimPlugin
plugin = wrapPlugin $ Plugin
  { environment = ()
  , exports = [ $(function' 'helloWorld) Sync ]
  }
