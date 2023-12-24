module Main where

import Calculator
import Data.Maybe (fromMaybe)
import System.Console.Haskeline

welcomeString :: String
welcomeString =
    " _                      _       _             \n\
    \| |                    | |     | |            \n\
    \| |__   __ _  ___ _   _| | __ _| |_ ___  _ __ \n\
    \| '_ \\ / _` |/ __| | | | |/ _` | __/ _ \\| '__|\n\
    \| | | | (_| | (__| |_| | | (_| | || (_) | |   \n\
    \|_| |_|\\__,_|\\___|\\__,_|_|\\__,_|\\__\\___/|_|\n\n"

loop :: InputT IO ()
loop =
    getInputLine "> "
        >>= outputStrLn
        . either id show
        . calculate
        . fromMaybe (error "bye!") -- TODO: exit more gracefully
        >> loop

main :: IO ()
main = putStr welcomeString >> runInputT defaultSettings loop
