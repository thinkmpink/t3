module PGame
    ( cmd
    , runCommand
    ) where

import Control.Applicative (liftA2)
import Game (GameInteraction, GameState, Player(..), Response, addPlayer,
            showBoard)
import Text.ParserCombinators.Parsec (GenParser, alphaNum, count, letter,
            many1, spaces, string, (<|>), (<?>))

type BoardSize = Int
type UserName = String
type Mark = Char
type Column = Int
type Row = Int
type NumberOfMoves = Int

data Command
    = ShowBoard
    | AddPlayer Player
    -- | SetBoardSize BoardSize
    -- | Undo NumberOfMoves
    -- | PickSpot Column Row

runCommand :: Command -> GameInteraction -> GameInteraction
runCommand ShowBoard      = showBoard
runCommand (AddPlayer p)  = addPlayer p

cmd :: GenParser Char () Command
cmd = spaces *> cmdAndArgs
            <?> "Command and any arguments"

cmdAndArgs :: GenParser Char () Command
cmdAndArgs = ShowBoard <$ pShowBoard
         <|> AddPlayer <$> pAddPlayer
         <?> "Available commands"

cmdName :: String -> GenParser Char () String
cmdName s = string s <* spaces

pShowBoard :: GenParser Char () String
pShowBoard = cmdName "showBoard"

pAddPlayer :: GenParser Char () Player
pAddPlayer = Person <$> (cmdName "addPlayer" *> pUserName) <*> pMark

pUserName :: GenParser Char () UserName
pUserName = many1 alphaNum <* spaces

pMark :: GenParser Char () Mark
pMark = letter <* spaces
-- data Tile a
--   = Corner a (Tile a) (Tile a) (Tile a)
--   | Leaf a
--
-- class AsChar a where
--   asChar :: a -> Char
--
-- pretty :: AsChar a => Tile a -> Tile (String, String, String)
-- pretty (Leaf a)             = Leaf ("   |",
--                                     ' ':(asChar a):" |",
--                                     "___|")
--
-- pretty (Corner a t1 t2 t3)  = Corner $ ( "____"
--                                        , "| " ++ (asChar a :" ")
--                                        , "|   "
--                                        ) (pretty t1) (pretty t2) (pretty t3)
--
-- prettyPrint :: Tile (String, String, String) -> String
-- prettyPrint (Corner (t, m, b) t1 t2 t3) = t ++ (prettyPrint t1)
--                                         ++ m ++ (prettyPrint t2)
--                                         ++ b ++ (prettyPrint t3)
-- prettyPrint (Leaf (t, m, b)) =
