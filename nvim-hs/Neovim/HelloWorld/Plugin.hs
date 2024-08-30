module Neovim.HelloWorld.Plugin (plugin) where

import           Neovim
import           Neovim.HelloWorld.Plugin.Core (echoBufNameCmd, helloWorld,
                                                helloWorldCmd, helloWorldCmd2)

plugin :: Neovim () NeovimPlugin
plugin = wrapPlugin $ Plugin
  { environment = ()
  , exports = [ $(function' 'helloWorld) Sync
              , $(command "HelloWorld" 'helloWorldCmd) [CmdSync Async]
              , $(command "HelloWorld2" 'helloWorldCmd2) [CmdSync Async]
              , $(command "EchoBufName" 'echoBufNameCmd) [CmdSync Async]
              ]
  }
