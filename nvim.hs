import           Neovim

import qualified Neovim.Example.Plugin as Example

main :: IO ()
main = do
  neovim defaultConfig
    { plugins = plugins defaultConfig ++ [ Example.plugin ]
    }
