module Main where

import Lib
import Game (GameInteraction, respond, startGame)
import GameCommand (Command, runCommand, cmd)
import Text.ParserCombinators.Parsec (ParseError, parse)
import Pipes ((>->), runEffect)
import qualified Pipes.Prelude as P


main :: IO ()
main = runEffect $ P.stdinLn
               >-> P.takeWhile (/= "quit")
               >-> P.map parseCmd
               >-> P.scan reportOrRun startGame respond
               >-> P.stdoutLn


parseCmd :: String -> Either ParseError Command
parseCmd = parse cmd "(stdin)"


reportOrRun :: GameInteraction
            -> Either ParseError Command
            -> GameInteraction
reportOrRun (st, msg) (Left e)  = (st, "Error parsing input:\n" ++ (show e))
reportOrRun g         (Right c) = runCommand c g
