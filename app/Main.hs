module Main where

import Calculator
import Control.Monad
import System.IO

welcomeString :: String
welcomeString =
    " _                      _       _             \n\
    \| |                    | |     | |            \n\
    \| |__   __ _  ___ _   _| | __ _| |_ ___  _ __ \n\
    \| '_ \\ / _` |/ __| | | | |/ _` | __/ _ \\| '__|\n\
    \| | | | (_| | (__| |_| | | (_| | || (_) | |   \n\
    \|_| |_|\\__,_|\\___|\\__,_|_|\\__,_|\\__\\___/|_|\n\n"

main :: IO ()
main =
    putStr welcomeString
        >> forever
            ( do
                putStr "> "
                hFlush stdout
                getLine >>= putStrLn . either id show . calculate . filter (/= ' ')
            )
