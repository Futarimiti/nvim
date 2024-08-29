import           Neovim

import qualified Neovim.Example.Plugin    as Example
import qualified Neovim.HelloWorld.Plugin as HelloWorld

main :: IO ()
main = neovim config

config :: NeovimConfig
config = defaultConfig
  { plugins = Example.plugin : HelloWorld.plugin : defaultConfig.plugins }
