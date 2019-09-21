module PGame
    ( withParser
    -- , run
    ) where

import Options.Applicative (Parser, ParserInfo, info, helper, progDesc,
                            argument, execParser)

type BoardSize = Int
type UserName = String
type Mark = Char
type Column = Int
type Row = Int
type NumberOfMoves = Int

data Command
    = SetBoardSize BoardSize
    | AddPlayer UserName Mark
    | ShowBoard
    | Undo NumberOfMoves
    | PickSpot Column Row

data Options = Options Command

withParser = execParser
  (parseOptions `withInfo` "Interact with the T3 Game API")

parseOptions :: Parser Options
parseOptions = undefined

-- run :: Options -> IO (Game String)
-- run opts = undefined

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- parseSetBoardSize :: Parser Command
-- parseSetBoardSize = SetBoardSize
    -- <$> argument

data Tile a
  = Corner a (Tile a) (Tile a) (Tile a)
  | Leaf a

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
