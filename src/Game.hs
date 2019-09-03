{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Game ( GameState
            , Game
            )

where

import Control.Monad.State ( State )
import Control.Zipper ( (:>>), Top )
import Data.Vector ( MVector )

newtype GameState a = GS (State Game a)
  deriving ( Functor, Applicative, Monad )

data Game = Game { stage :: Stage
                 , players :: [Player]
                 , lattice :: Top :>> Lattice :>> Tac
                 }

-- addPlayer :: String -> Char -> GameState Player
-- addPlayer g nm mk = undefined

data Stage = Start | Continue | End
  deriving ( Show, Eq, Ord )

-- | The board on which the game is played.
newtype Lattice = MVector Tac

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
