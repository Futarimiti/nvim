module Neovim.Example.Plugin.Fibonacci
  (fibonacci) where

import           Neovim

-- | All fibonacci numbers.
fibonacciNumbers :: [Integer]
fibonacciNumbers = 0:fibs -- Since were using !! to index an element in a list, we need a 0 in front
    where
      fibs :: [Integer]
      fibs = 1:scanl1 (+) fibs

-- | Neovim is not really good with big numbers, so we return a 'String' here.
fibonacci :: Int -> Neovim env String
fibonacci n = return . show $ fibonacciNumbers !! n
