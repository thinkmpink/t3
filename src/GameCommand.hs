module GameCommand
    ( Command
    , cmd
    , runCommand
    ) where

import Control.Applicative (liftA2)
import Text.ParserCombinators.Parsec (GenParser, alphaNum, choice, count,
            digit, letter, many1, spaces, string, try, (<|>), (<?>))
import qualified Game as G


data Command
    = AddPlayer    G.Player
    | SetBoardSize G.BoardSize
    | ShowBoard
    | ShowCommands
    | PickSpot     G.Column G.Row
    | Undo         G.NumberOfMoves
    | WhoseTurn

-- | Run any command
runCommand :: Command -> G.GameInteraction -> G.GameInteraction
runCommand ShowBoard        = G.showBoard
runCommand (AddPlayer p)    = G.addPlayer p
runCommand (SetBoardSize b) = G.setBoardSize b
runCommand ShowCommands     = G.showCommands
runCommand (PickSpot c r)   = G.pickSpot c r
runCommand WhoseTurn        = G.whoseTurn

cmd :: GenParser Char () Command
cmd = spaces *> cmdAndArgs
            <?> "Command and any arguments"

cmdAndArgs :: GenParser Char () Command
cmdAndArgs = AddPlayer <$> pAddPlayer
         <|> PickSpot <$> (pPickSpot *> pCol) <*> pRow
         <|> try (ShowBoard <$ pShowBoard)
         <|> try (ShowCommands <$ pShowCommands)
         <|> SetBoardSize <$> pSetBoardSize
         <|> WhoseTurn <$ pWhoseTurn
         <?> "Available commands"

cmdName :: String -> GenParser Char () String
cmdName s = string s <* spaces

pShowBoard :: GenParser Char () String
pShowBoard = cmdName "showBoard"

pShowCommands :: GenParser Char () String
pShowCommands = cmdName "showCommands"

pWhoseTurn :: GenParser Char () String
pWhoseTurn = cmdName "whoseTurn"

pAddPlayer :: GenParser Char () G.Player
pAddPlayer = G.Person <$> (cmdName "addPlayer" *> pUserName) <*> pMark

pUserName :: GenParser Char () G.UserName
pUserName = many1 alphaNum <* spaces

pMark :: GenParser Char () G.Mark
pMark = letter <* spaces

pSetBoardSize :: GenParser Char () G.BoardSize
pSetBoardSize = cmdName "setBoardSize" *> pBoardSize <* spaces

pBoardSize :: GenParser Char () G.BoardSize
pBoardSize = pInt

pPickSpot :: GenParser Char () String
pPickSpot = cmdName "pickSpot"

pCol :: GenParser Char () G.Column
pCol = pInt <* spaces

pRow :: GenParser Char () G.Column
pRow = pInt <* spaces

pInt :: GenParser Char () Int
pInt = read <$> many1 digit
