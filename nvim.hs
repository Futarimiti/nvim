import           Neovim

import qualified Neovim.Example.Plugin as Example

main :: IO ()
main = neovim config

config :: NeovimConfig
config = defaultConfig { plugins = Example.plugin : defaultConfig.plugins }
