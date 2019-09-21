module Main where

import Lib
import Game (route, respond, startGame)

main :: IO ()
main = interact (unlines
              . (map respond . scanl route startGame)
              . lines)
