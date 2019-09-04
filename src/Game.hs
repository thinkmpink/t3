{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Game ( GameState
            , Game
            , Stage
            , Lattice
            , defaultGame
            , addPlayer
            , newGame
            , newLattice -- remove on impl print
            , stage -- remove on impl print
            , players -- remove on impl print
            , lattice -- remove on impl print
            , get -- remove on impl print
            , runState -- remove on impl print
            , execState -- remove on impl print
            , evalState -- remove on impl print
            )

where

import Control.Monad.State ( State, get, put, runState, execState, evalState )
-- import Control.Zipper ( (:>>), Top )
import Data.Set ( fromList )
import Data.List ( intercalate, intersperse )
import qualified Data.Vector as V

newtype GameState a = GS (State Game a)
  deriving ( Functor, Applicative, Monad )

data Game = Game { stage :: Stage
                 , players :: [Player]
                 , lattice :: Lattice
                 -- , lattice :: Top :>> Lattice :>> Tac
                 }

defaultGame :: Game
defaultGame = Game Start [] (newLattice 3)

newGame :: Int -> [(String, Char)] -> Either String (GameState ())
newGame dim ps
  | dim < 1
      = Left ("Invalid lattice size: " ++ (show dim)
                      ++ ". Board must be at least size 1.")
  | let marks  = map snd ps
        sMarks = fromList marks
    in length marks /= length sMarks
      = Left ("Duplicated marks found. All players must have unique marks.")
  | otherwise = let starters = map (\(n, m) -> Player n m) ps
                    l = newLattice dim
                    game = Game Start starters l
                in Right $ GS $ put game


addPlayer :: GameState a -> String -> Char -> GameState (Either String Player)
addPlayer gs nm mk = GS $ get >>= \game ->
  let p   = Player nm mk
      ps  = players game
  in case any (p ==) ps of
      True  -> return (Left $ "Player "++ nm ++ " already exists.")
      False -> put game { players = (p:ps) } >> return (Right p)

data Stage = Start | Continue | End
  deriving ( Show, Eq, Ord )

-- | The board on which the game is played.
-- For a 2x2 board with 2 players (E and M) this might look like:
-- ---------
-- | E | M |
-- |---|---|
-- | E |   |
-- ---------
newtype Lattice = Lattice { vec :: V.Vector Tac } deriving ( Eq )

width :: Lattice -> Int
width l = let area = (fromIntegral . V.length . vec $ l) :: Double
          in (fromIntegral . round . sqrt $ area) :: Int

instance Show Lattice where
  show lattice =
    let v = vec lattice
        side = width lattice
        cap = take (4 * side + 1) $ repeat '-'
        level l = V.slice (l * side) side v
        tacMarks = V.toList . fmap getTacMark . level
        wrap l r center = l ++ center ++ r
        bookend = wrap "| " " |"
        levelStr = bookend . intercalate " | " . map (:[]) . tacMarks
        between = ('|':) . concat . take side $ repeat "---|"
        allLevels = map levelStr [0..(side-1)]
        uncapped = intersperse between allLevels
    in unlines $ [cap] ++ uncapped ++ [cap]

newLattice :: Int -> Lattice
newLattice n = Lattice $ V.replicate (n ^ 2) Nothing

-- | Populates the 'Lattice'. Either a player has reserved a slot in the
-- 'Lattice' via a Move or it is blank.
type Tac = Maybe Move

-- | Gets the Mark at a move or ' '
getTacMark :: Tac -> Char
getTacMark = maybe ' ' (mark . player)

data Move = Move { player :: Player
                 , strategy :: Strategy
                 } deriving ( Eq, Show )


data Strategy = Solicit deriving ( Eq, Show )

-- | A Tic Tac Toe player. Each player has a unique mark, which identifies their
-- 'Move's on the 'Lattice'.
data Player = Player { name :: String
                     , mark :: Char
                     } deriving ( Show )

instance Eq Player where
  p1 == p2 = mark p1 == mark p2
