module Main where

import Calculator
import Control.Monad
import Data.Maybe (fromMaybe)
import System.Console.Haskeline
import System.IO

welcomeString :: String
welcomeString =
    " _                      _       _             \n\
    \| |                    | |     | |            \n\
    \| |__   __ _  ___ _   _| | __ _| |_ ___  _ __ \n\
    \| '_ \\ / _` |/ __| | | | |/ _` | __/ _ \\| '__|\n\
    \| | | | (_| | (__| |_| | | (_| | || (_) | |   \n\
    \|_| |_|\\__,_|\\___|\\__,_|_|\\__,_|\\__\\___/|_|\n\n"

loop :: InputT IO ()
loop = getInputLine "> " >>= outputStrLn . either id show . calculate . fromMaybe "what" >> loop

main :: IO ()
main = putStr welcomeString >> runInputT defaultSettings loop
