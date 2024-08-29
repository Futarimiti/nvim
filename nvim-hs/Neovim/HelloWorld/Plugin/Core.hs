{-# LANGUAGE OverloadedStrings #-}

module Neovim.HelloWorld.Plugin.Core
  ( helloWorld
  , helloWorldCmd
  , helloWorldCmd2
  ) where

import           Data.String.Interpolate (i)
import           Neovim
import qualified Neovim.API.String       as API

helloWorld :: Neovim env String
helloWorld = pure "hello world"

helloWorldCmd :: CommandArguments -> Neovim env ()
helloWorldCmd = const $ API.nvim_echo chunk False mempty
  where chunk = [ObjectArray [ObjectString "hello world"]]

helloWorldCmd2 :: CommandArguments -> Neovim env ()
helloWorldCmd2 = const $ do
  _ <- API.nvim_exec [i|echo "#{text}"|] False
  pure ()
    where
      text :: String
      text = "hello world"
