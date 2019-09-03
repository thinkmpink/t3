{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Game ( GameState
            , Game
            , addPlayer
            , newGame
            )

where

import Control.Monad.State ( State, get, put )
-- import Control.Zipper ( (:>>), Top )
import Data.Set ( fromList )
import qualified Data.Vector as V

newtype GameState a = GS (State Game a)
  deriving ( Functor, Applicative, Monad )

data Game = Game { stage :: Stage
                 , players :: [Player]
                 , lattice :: Lattice
                 -- , lattice :: Top :>> Lattice :>> Tac
                 }

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
newtype Lattice = Lattice (V.Vector Tac)

newLattice :: Int -> Lattice
newLattice n = Lattice $ V.replicate (n ^ 2) Nothing
-- newLattice = undefined

-- | Populates the 'Lattice'. Either a player has reserved a slot in the
-- 'Lattice' via a Move or it is blank.
type Tac = Maybe Move

data Move = Move { player :: Player
                 , strategy :: Strategy
                 }


data Strategy = Solicit

-- | A Tic Tac Toe player. Each player has a unique mark, which identifies their
-- 'Move's on the 'Lattice'.
data Player = Player { name :: String
                     , mark :: Char
                     } deriving ( Show )

instance Eq Player where
  p1 == p2 = mark p1 == mark p2
