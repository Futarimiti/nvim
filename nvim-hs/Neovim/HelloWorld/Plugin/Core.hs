module Neovim.HelloWorld.Plugin.Core (helloWorld) where

import           Neovim

helloWorld :: Neovim env String
helloWorld = pure "hello world"
