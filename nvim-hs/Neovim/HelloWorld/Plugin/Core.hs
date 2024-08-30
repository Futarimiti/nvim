{-# LANGUAGE OverloadedStrings #-}

module Neovim.HelloWorld.Plugin.Core
  ( helloWorld
  , helloWorldCmd
  , helloWorldCmd2
  , echoBufNameCmd
  ) where

import           Data.String.Interpolate (i)
import           Neovim
import qualified Neovim.API.String       as API
import           Text.RawString.QQ       (r)

helloWorld :: Neovim env String
helloWorld = pure "hello world"

helloWorldCmd :: CommandArguments -> Neovim env ()
helloWorldCmd = const $ API.nvim_echo chunk False mempty
  where chunk = [ObjectArray [ObjectString "hello world"]]

helloWorldCmd2 :: CommandArguments -> Neovim env ()
helloWorldCmd2 = const $ do
  let text = "hello" :: String
  _ <- API.nvim_exec [r|echomsg
           \ "this message will be captured,"
           \ "therefore won't be printed,"
           \ "nor appear in :messages"|] True
  "" <- API.nvim_exec [i|echomsg "#{text}"|] False
  "" <- API.nvim_exec [r|echomsg "world"|] False
  pure ()

echoBufNameCmd :: CommandArguments -> Neovim env ()
echoBufNameCmd = const $ do
  buf <- API.vim_get_current_buffer
  name <- API.buffer_get_name buf
  _ <- API.nvim_exec [i|echo "#{name}"|] False
  pure ()
