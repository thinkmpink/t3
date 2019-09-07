module Main where

import Lib
import Game (Game, runGame, welcomeToT3)

main :: IO ()
main = runGame welcomeToT3 3 >>
       putStrLn "Thanks for playing!"
