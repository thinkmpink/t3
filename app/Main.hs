module Main where

import Lib
import Game                           (GameInteraction, respond, startGame)
import PGame                          (runCommand, cmd)
import Text.ParserCombinators.Parsec  (parse)

-- main :: IO ()
-- main = interact (unlines
--               . (map respond . scanl route startGame)
--               . lines)

main :: IO ()
main = do let g = startGame
          putStrLn $ respond g
          gameInteract g
          return ()


gameInteract :: GameInteraction -> IO GameInteraction
gameInteract g = do
    putStr "#: "
    c <- getLine
    case parse cmd "(stdin)" c of
        Left e -> do
            putStrLn "Error parsing input:"
            print e
            gameInteract g
        Right command -> do
            let newInteraction = runCommand command startGame
            putStrLn $ respond newInteraction
            gameInteract newInteraction
