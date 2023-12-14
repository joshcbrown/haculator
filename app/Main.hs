module Main where

import Control.Monad
import qualified MyLib (someFunc)

main :: IO ()
main = forever $ do
    putStrLn "Hello, Haskell!"
    MyLib.someFunc
