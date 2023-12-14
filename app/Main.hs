module Main where

import Calculator
import Control.Monad
import System.IO

main :: IO ()
main = forever $ do
    putStr "> "
    hFlush stdout
    getLine >>= print . either id show . calculate . filter (/= ' ')
