module Main where

import Lib
import Game (route, respond, startGame)

-- route :: GameState -> String -> GameState
-- respond :: GameState -> String

main :: IO ()
main = interact (unlines
              . (map respond . scanl route startGame)
              . lines)

-- main :: IO ()
-- main = interact (unlines . (map show . scanl myAdd 0) . lines)
--
-- myAdd :: Int -> String -> Int
-- myAdd acc s = acc + (read s :: Int)
