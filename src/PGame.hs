module PGame
    ( Command
    , cmd
    , runCommand
    ) where

import Control.Applicative (liftA2)
import Game (GameInteraction, GameState, Player(..), Response, addPlayer,
            pickSpot, setBoardSize, showBoard)
import Text.ParserCombinators.Parsec (GenParser, alphaNum, choice, count,
            digit, letter, many1, spaces, string, try, (<|>), (<?>))

type BoardSize = Int
type UserName = String
type Mark = Char
type Column = Int
type Row = Int
type NumberOfMoves = Int

data Command
    = ShowBoard
    | AddPlayer    Player
    | SetBoardSize BoardSize
    | PickSpot     Column Row
    -- | Undo NumberOfMoves

-- | Run any command
runCommand :: Command -> GameInteraction -> GameInteraction
runCommand ShowBoard        = showBoard
runCommand (AddPlayer p)    = addPlayer p
runCommand (SetBoardSize b) = setBoardSize b
runCommand (PickSpot c r)   = pickSpot c r

cmd :: GenParser Char () Command
cmd = spaces *> cmdAndArgs
            <?> "Command and any arguments"

cmdAndArgs :: GenParser Char () Command
cmdAndArgs = AddPlayer <$> pAddPlayer
         <|> PickSpot <$> (pPickSpot *> pCol) <*> pRow
         <|> try (ShowBoard <$ pShowBoard)
         <|> SetBoardSize <$> pSetBoardSize
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

pSetBoardSize :: GenParser Char () BoardSize
pSetBoardSize = cmdName "setBoardSize" *> pBoardSize <* spaces

pBoardSize :: GenParser Char () BoardSize
pBoardSize = pInt

pPickSpot :: GenParser Char () String
pPickSpot = cmdName "pickSpot"

pCol :: GenParser Char () Column
pCol = pInt <* spaces

pRow :: GenParser Char () Column
pRow = pInt <* spaces

pInt :: GenParser Char () Int
pInt = read <$> many1 digit
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
