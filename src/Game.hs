{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Game ( Game
            , GameState(..)
            , GameConfig
            , LatticeCoordinate(..)
            , fromCoordinate
            , defaultGame
            )

where

import Control.Monad.Except     (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class   (MonadIO, liftIO)
import Control.Monad.Reader     (ReaderT, runReaderT, ask)
import Control.Monad.State      (StateT, get, put, runState, gets, modify,
                                runStateT)
import Data.List                (find, intercalate, intersperse )
import Data.Set                 (fromList)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.IO                (IO, putStr, putStrLn, getLine)


newtype Game a = G {
    runG :: ExceptT GameException (StateT GameState (ReaderT GameConfig IO)) a
  } deriving ( Functor, Applicative, Monad, MonadIO )

data GameState = Start Lattice
               | Turn Lattice Player
               | Done Lattice [PlayerResult]

-- runGame :: Game a -> Int -> IO (a, GameState)
-- runGame g dim =
--   let state = defaultGame dim
--   in runStateT (runExceptT (runG g)) state

instance Show GameState where
  show (Start l)    = "Beginning of game.\n" ++ (prettyLattice l)
  show (Turn l p)   = "It's " ++ (userName p) ++ "'s turn.\n"
                      ++ (prettyLattice l)
  show (Done l rs)  = "Game over! " ++ (showResults rs)
                      ++ "\nThe final board:\n" ++ (prettyLattice l)

defaultGame :: Int -> GameState
defaultGame = Start . newLattice

type PlayerResult = (Player, Result)

showResults :: [PlayerResult] -> String
showResults = unlines . map showResult

showResult :: PlayerResult -> String
showResult (p, Win) = (userName p) ++ " won!"
showResult (p, Tie) = (userName p) ++ " tied."
showResult (p, Lose) = (userName p) ++ " lost."

winner :: [PlayerResult] -> Maybe Player
winner rs = find (\(_, r) -> r == Win) rs >>= return . fst

data Result = Win | Lose | Tie deriving (Eq, Show)

data GameException = DuplicatePlayer Player
                   | NotEnoughPlayers [Player]
                   | CoordinateOutOfBounds LatticeCoordinate Lattice
                   | CoordinateOccupied LatticeCoordinate Lattice Player Player
                   | InvalidDim Int
                   | MarkTooShort String
                   | NameTooShort UserName
                   | MarkTooLong String


data GameConfig = GameConfig {
    players :: [Player]
  , dim :: Int
} deriving (Eq, Show)

type VCoord = Int
type UCoord = (Int, Int)

data LatticeCoordinate = UserCoordinate UCoord
                       | VectorCoordinate VCoord deriving (Eq, Show)

-- 3 (2, 2) -> 4
-- 3 (1, 1) -> 0
fromCoordinate :: Int -> LatticeCoordinate -> VCoord
fromCoordinate dim (UserCoordinate (x, y)) = (y - 1) * dim + (x - 1)
fromCoordinate dim (VectorCoordinate v) = v

-- | The board on which the game is played.
-- For a 2x2 board with 2 players (E and M) this might look like:
-- ---------
-- | E | M |
-- |---|---|
-- | E |   |
-- ---------
newtype Lattice = Lattice { vec :: V.Vector Tac } deriving (Eq, Show)

-- addTac :: Tac -> Coordinate -> Lattice -> Lattice
-- addTac tac coord lattice =
  -- let i = fromCoordinate coord



prettyLattice :: Lattice -> String
prettyLattice lattice =
  let v = vec lattice
      side = width lattice
      cap = take (4 * side + 1) $ repeat '-'
      level l = V.slice (l * side) side v
      tacMarks = V.toList . fmap tacMark . level
      wrap l r center = l ++ center ++ r
      bookend = wrap "| " " |"
      levelStr = bookend . intercalate " | " . map (:[]) . tacMarks
      between = ('|':) . concat . take side $ repeat "---|"
      allLevels = map levelStr [0..(side-1)]
      uncapped = intersperse between allLevels
  in unlines $ [cap] ++ uncapped ++ [cap]

width :: Lattice -> Int
width l = let area = (fromIntegral . V.length . vec $ l) :: Double
          in (fromIntegral . round . sqrt $ area) :: Int

newLattice :: Int -> Lattice
newLattice n = Lattice $ V.replicate (n ^ 2) Open

-- | Populates the 'Lattice'.
data Tac = Mark Player | Open
  deriving (Eq)

instance Show Tac where
  show t = show . tacMark $ t

type Mark = Char
type UserName = String

emptyMark :: Mark
emptyMark = ' '

-- | Gets the Mark at a 'Move' or ' '
tacMark :: Tac -> Mark
tacMark (Mark p) = mark p
tacMark Open = emptyMark


-- data Strategy = RandomMove deriving (Eq, Show)

-- | A Tic Tac Toe player. Each player has a unique mark, which identifies their
-- 'Move's on the 'Lattice'.
data Player = Person UserName Mark
            -- | Computer UserName Mark Strategy
            deriving (Show)

mark :: Player -> Char
mark (Person _ m) = m
-- mark (Computer _ m _) = m

userName :: Player -> String
userName (Person u _) = u
-- userName (Computer u _ _) = u

instance Eq Player where
  p1 == p2 = mark p1 == mark p2



  -- newGame :: Int -> [(String, Char)] -> Game GameState
  -- newGame dim ps
  --   | dim < 1
  --       = Left ("Invalid lattice size: " ++ (show dim)
  --                       ++ ". Board must be at least size 1.")
  --   | let marks  = map snd ps
  --         sMarks = fromList marks
  --     in length marks /= length sMarks
  --       = Left ("Duplicated marks found. All players must have unique marks.")
  --   | otherwise = let starters = map (\(n, m) -> Player n m) ps
  --                     l = newLattice dim
  --                     game = GameState Start starters l
  --                 in Right $ GS $ put game


  -- addPlayer :: Game Player
  -- addPlayer = liftIO $ putStrLn "Would you like to add a Player?" >>
  --             gets (map name . players) >>= \names ->
  --             liftIO (putStrLn ("You currently have " ++ (show . length $ names)
  --                       ++ "players: " ++ (show names))) >>
  --             liftIO (putStrLn "To add a player, enter a player name: ") >>
  --             liftIO getLine >>= \nm ->
  --             if null nm
  --               then throwError "The name must have at least one letter."
  --               else return nm >>
  --             liftIO $ putStrLn ("We also need a mark to represent the player"
  --                       ++ "on the board. It should be 1 character long: ") >>
  --             liftIO getLine >>= \mark ->
  --             let trimmed@(x:xs) = T.unpack . T.strip . T.pack $ mark
  --             in if null trimmed
  --                  then throwError "The mark must not be blank."
  --                  else if not (null xs)
  --                    then throwError "The mark must be exactly 1 character."
  --                    else return x >>= \mk ->
  --             gets players >>= \ps ->
  --             let p = Player nm mk
  --             in if any (p ==) ps
  --                  then throwError ("Player " ++ (show p) ++ " already exists.")
  --                  else return (modify (\g -> g { players = (p:(players g)) })) >>
  --             return p
