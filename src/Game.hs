module Game
    ( route
    , respond
    , startGame
    ) where

import Control.Monad            (join)
import Data.List                (find, intercalate, intersperse, uncons)
import Data.Maybe               (fromMaybe, fromJust)
import Text.Read                (readMaybe)
import Data.Monoid              ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V


type Request = String
type Param = String
type Response = String
type GameInteraction = (GameState, Response)

route :: GameInteraction -> Request -> GameInteraction
route i r = routeReq i (parseReqParams . sanitize $ r)

routeReq :: GameInteraction -> (Request, [Param]) -> GameInteraction
routeReq c ("setBoardSize", ps) = setBoardSize ps c
routeReq c ("addPlayer", ps)    = addPlayer ps c
routeReq c ("showBoard", ps)    = showBoard c
routeReq c ("pickSpot", ps)     = pickSpot ps c
routeReq c (r, _)               = requestNotFound c r


sanitize :: Request -> Request
sanitize = T.unpack . T.strip . T.pack

parseReqParams :: Request -> (Request, [Param])
parseReqParams r = fromMaybe ("", []) (uncons . words $ r)


respond :: GameInteraction -> Response
respond (_, s) = s

startGame :: GameInteraction
startGame = (Start, startMessage)

startMessage :: Response
startMessage = unlines $ [welcomeMessage, apiDescription, howToExit]

requestNotFound :: GameInteraction -> Request -> GameInteraction
requestNotFound (state, _) r = (state, "Request not found: " ++ r ++ ".")

data GameState = Start
               | Turn Lattice [Player] GameState
               | Done Lattice [PlayerResult] GameState


showBoard :: GameInteraction -> GameInteraction
showBoard (state, _) = (state, prettyLattice (getLattice state))

addPlayer :: [Param] -> GameInteraction -> GameInteraction
addPlayer (p:_) (state, _) = configPlayers p state
addPlayer s (state, _)     = (state, invalidParams ("addPlayer", s))

configPlayers :: String -> GameState -> GameInteraction
configPlayers newStr oldState =
  let newPlayer   = readMaybe newStr :: Maybe (UserName, Mark)
      currPlayers = getPlayers oldState
      currMarks   = map mark currPlayers
      l           = getLattice oldState
      parseFailed = (oldState, parseFail newStr)
      checkUserExists (u, m)
                  = let p         = Person u m
                        newState  = Turn l (p:currPlayers) oldState
                    in if m `elem` currMarks
                         then (oldState, duplicatePlayer (u, m))
                         else (newState, successAddPlayer p)
  in maybe parseFailed checkUserExists newPlayer

duplicatePlayer :: (UserName, Mark) -> Response
duplicatePlayer (u, m) = "Could not add player for username '" ++ u
                         ++ "' with mark '" ++ (m:"'.")
                         ++ "No two players in a game may use the same mark."

successAddPlayer :: Player -> Response
successAddPlayer p = "Successfully added player " ++ (userName p) ++ "."

defaultSize :: Int
defaultSize = 3

getPlayers :: GameState -> [Player]
getPlayers Start = default2Players
getPlayers (Turn _ ps _) = ps

getBoardSize :: GameState -> Int
getBoardSize Start          = defaultSize
getBoardSize (Turn l _ _)   = width l

getLattice :: GameState -> Lattice
getLattice Start          = newLattice $ getBoardSize Start
getLattice (Turn l _ _)   = l

rotate :: Int -> [a] -> [a]
rotate = drop <> take

setBoardSize :: [Param] -> GameInteraction -> GameInteraction
-- setBoardSize (Turn, _) = (Prompt losehistory)
setBoardSize (p:_) (state, _)  = configBoardSize p state
setBoardSize s (state, _)      = (state, invalidParams ("setBoardSize", s))

configBoardSize :: Param -> GameState -> GameInteraction
configBoardSize newStr oldState =
  let newSize = readMaybe newStr :: Maybe Int
      parseFailed = (oldState, parseFail newStr)
      ps = getPlayers oldState
      checkDim i = if i < 1
                      then (oldState, dimTooSmall i)
                      else (Turn (newLattice i) ps oldState,
                            successSizeChange i)
  in maybe parseFailed checkDim newSize

dimTooSmall :: Int -> Response
dimTooSmall i = "Could not set board size to: " ++ (show i)
                ++ ". The board size must be greater than 0."

successSizeChange :: Int -> Response
successSizeChange i = "Successfully set board size to " ++ (show i) ++ "."

parseFail :: String -> Response
parseFail s = "Could not parse input: " ++ s

invalidParams :: (Request, [Param]) -> Response
invalidParams (r, ps) = "Invalid parameters " ++ (show ps)
                        ++ " for request " ++ r


pickSpot :: [Param] -> GameInteraction -> GameInteraction
pickSpot (p:_) (state, _) = let ucoord    = readMaybe p :: Maybe UCoord
                                claim     = fmap UserCoordinate ucoord
                                badParse = (state, parseFail p)
                            in maybe badParse (claimSpot state) claim
pickSpot ps (state, _)    = (state, invalidParams ("pickSpot", ps))

claimSpot :: GameState -> LatticeCoordinate -> GameInteraction
claimSpot curr spot
  | isDone curr            = alreadyDoneResponse curr
  | isOutOfBounds spot l   = outOfBoundsResponse spot curr
  | taker /= Nothing       = takenResponse (fromJust taker) spot curr
  | isWinningMove p spot l = winsResponse        spot curr
  | otherwise              = acceptClaimResponse spot curr
  where l      = getLattice curr
        taker  = getTaker spot l
        p      = head . getPlayers $ curr

isDone :: GameState -> Bool
isDone (Done _ _ _) = True
isDone _            = False

alreadyDoneResponse :: GameState -> GameInteraction
alreadyDoneResponse s = (s, "This game is over. No more moves are allowed.")


isOutOfBounds :: LatticeCoordinate -> Lattice -> Bool
isOutOfBounds c l = let v   = toVCoordinate (width l) c
                        len = length . vec $ l
                    in 0 <= v && v <= len

outOfBoundsResponse :: LatticeCoordinate
                    -> GameState
                    -> GameInteraction
outOfBoundsResponse c g = (g, r)
  where r = "Coordinate entered is out of the bounds of the game: "
            ++ (show c) ++ ".\nChoose a coordinate within "
            ++ (showBounds . getLattice $ g)

getTaker :: LatticeCoordinate -> Lattice -> Maybe Player
getTaker c l = join $ (vec l) V.!? (toVCoordinate (width l) c)

takenResponse :: Player
              -> LatticeCoordinate
              -> GameState
              -> GameInteraction
takenResponse p c g = (g, r)
  where r = "Request to claim board spot "
            ++ (show $ toUserCoordinate (width . getLattice $ g) c)
            ++ " is not permitted.\nThis spot is already taken  by "
            ++ (userName p) ++ "."

isWinningMove :: Player -> LatticeCoordinate -> Lattice -> Bool
isWinningMove p c l =
  let newLattice = addTac p c l
      row        = ownsRow p c l
      col        = ownsColumn p c l
      ld         = ownsDescDiag p c l
      rd         = ownsAscDiag p c l
  in row || col || ld || rd

winsResponse :: LatticeCoordinate
             -> GameState
             -> GameInteraction
winsResponse = undefined

acceptClaimResponse :: LatticeCoordinate
                    -> GameState
                    -> GameInteraction
acceptClaimResponse = undefined

      -- validate game state
      -- validate claim
      -- update lattice
      -- compute successful move message
      -- if state is terminal, add finished message



-- data ClaimResponse = OutOfBounds LatticeCoordinate Lattice
--                    | Taken Player LatticeCoordinate Lattice
--                    | Available GameState LatticeCoordinate Lattice

-- tryClaim :: Coordinate -> GameState -> ClaimResponse
-- tryClaim c g
--   | not $ isInBounds c l    = OutOfBounds c l
--   | taken /= Nothing        = Taken (fromJust taken) c l
--   | gameRoundup /= Nothing  = Available (fromJust gameRoundup) c l
--   | otherwise               = Available (nextLattice)
--   where l           = getLattice l
--         taken       = checkTaken c l
--         nextLattice = runClaim c l
--         gameRoundup = roundup nextLattice

-- | Coordinates of the board
type VCoord = Int
type UCoord = (Int, Int)

data LatticeCoordinate = UserCoordinate UCoord
                       | VectorCoordinate VCoord
                       deriving (Eq, Show)

toVCoordinate :: Int -> LatticeCoordinate -> VCoord
toVCoordinate _ (VectorCoordinate v) = v
toVCoordinate dimension (UserCoordinate (x, y)) = (y - 1) * dimension + (x - 1)

toUserCoordinate :: Int -> LatticeCoordinate -> UCoord
toUserCoordinate _ (UserCoordinate u) = u
toUserCoordinate dimension (VectorCoordinate v) =
  let x = v `div` dimension + 1
      y = v `mod` dimension + 1
  in (x, y)

showBounds :: Lattice -> String
showBounds l =
  let dimension = width l
  in "(1 <= number of columns <= " ++ (show dimension)
     ++ ", 1 <= number of rows <= " ++ (show dimension) ++ ")"

-- | The board on which the game is played.
-- For a 2x2 board with 2 players (E and M) this might look like:
-- ---------
-- | E | M |
-- |---|---|
-- | E |   |
-- ---------
newtype Lattice = Lattice { vec :: V.Vector Tac } deriving (Eq, Show)

addTac :: Player -> LatticeCoordinate -> Lattice -> Lattice
addTac p c l = let tac = Just p
                   i   = toVCoordinate (width l) c
               in Lattice $ (vec l) V.// [(i, tac)]

owns :: Player -> Tac -> Bool
owns p (Just q) = p == q
owns _ Nothing = False

ownsRow :: Player -> LatticeCoordinate -> Lattice -> Bool
ownsRow p c l =
  let w   = width l
      i   = toVCoordinate w c
      row = V.slice i w (vec l)
  in V.all (owns p) row


ownsColumn :: Player -> LatticeCoordinate -> Lattice -> Bool
ownsColumn p c l =
  let w   = width l
      i   = toVCoordinate w c
      col = V.ifilter (\ix _ -> ix `mod` w == i `mod` w) $ vec l
  in V.all (owns p) col

ownsDescDiag :: Player -> LatticeCoordinate -> Lattice -> Bool
ownsDescDiag p c l =
  let w   = width l
      onDiag i _ = let (rx, cx) = toUserCoordinate w (VectorCoordinate i)
                   in rx == cx
      diag = V.ifilter onDiag $ vec l
  in V.all (owns p) diag

ownsAscDiag :: Player -> LatticeCoordinate -> Lattice -> Bool
ownsAscDiag = undefined

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
newLattice n = Lattice $ V.replicate (n ^ 2) Nothing


-- | Populates the 'Lattice'.
type Tac = Maybe Player
type Mark = Char
type UserName = String

emptyMark :: Mark
emptyMark = ' '

-- | Gets the Mark at a 'Tac' or ' '
tacMark :: Tac -> Mark
tacMark = maybe emptyMark mark


default2Players :: [Player]
default2Players = [ Person "Player 1" '1'
                  , Person "Player 2" '2'
                  ]

-- -- data Strategy = RandomMove deriving (Eq, Show)
--

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


welcomeMessage :: Response
welcomeMessage = "Welcome to T3: Tic Tac Toe in Haskell!"

apiDescription :: Response
apiDescription =
  intercalate "\n  " [ "The game supports the following commands:"
                     , "setBoardSize <size>"
                     , "addPlayer <(username, single-letter-mark)>"
                     , "pickSpot <(column, row)>"
                     , "showBoard"
                     ]

howToExit :: Response
howToExit = "Press <Ctrl-D> to quit."




type PlayerResult = (Player, Result)

showResults :: [PlayerResult] -> String
showResults = unlines . map showResult

showResult :: PlayerResult -> String
showResult (p, Win) = (userName p) ++ " won!"
showResult (p, Tie) = (userName p) ++ " tied."
showResult (p, Lose) = (userName p) ++ " lost."

winner :: [PlayerResult] -> Maybe Player
winner rs = find (\(_, r) -> r == Win) rs >>= return . fst

data Result = Win | Lose | Tie
  deriving (Eq, Show)



-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeOperators #-}
--
-- module Game
--             ( Game
--             , runGame
--             , welcomeToT3
--             , GameState(..)
--             , GameConfig
--             , LatticeCoordinate(..)
--             , toVCoordinate
--             , newGame
--             , winner
--             )
--
-- where
--
-- import Control.Monad.Except     (ExceptT, MonadError, runExceptT, throwError,
--                                 catchError)
-- import Control.Monad.IO.Class   (MonadIO, liftIO)
-- import Control.Monad.Reader     (ReaderT, runReaderT)
-- import Control.Monad.State      (StateT, runStateT)
-- import Data.List                (find, intercalate, intersperse)
-- -- import Data.Set                 (fromList)
-- import qualified Data.Vector as V
-- import System.IO                (IO)
-- import Text.Read                (readMaybe)
--
--
-- newtype Game a = G {
--     runG :: ExceptT GameException (StateT GameState (ReaderT GameConfig IO)) a
--   } deriving ( Functor, Applicative, Monad, MonadIO )
--
-- data GameState = Start Lattice
--                | Turn Lattice Player
--                | Done Lattice [PlayerResult]
--
-- runGame :: Game a -> IO (Either GameException a, GameState)
-- runGame g =
--   let config = defaultConfig
--       state = newGame config
--   in runReaderT (runStateT (runExceptT (runG g)) state) config
--
-- instance Show GameState where
--   show (Start l)    = "Beginning of game.\n" ++ (prettyLattice l)
--   show (Turn l p)   = "It's " ++ (userName p) ++ "'s turn.\n"
--                       ++ (prettyLattice l)
--   show (Done l rs)  = "Game over! " ++ (showResults rs)
--                       ++ "\nThe final board:\n" ++ (prettyLattice l)
--
-- data GameException =
--     DuplicatePlayer Player [Player]
--   | NotEnoughPlayers [Player]
--   | CoordinateOutOfBounds LatticeCoordinate Lattice
--   | CoordinateOccupied LatticeCoordinate Lattice Player Player
--   | InvalidDim Int
--   | MarkTooShort String
--   | NameTooShort UserName
--   | MarkTooLong String
--   | APINotFound String
--   | StringParseFailed String
--   deriving (Eq)
--
-- instance Show GameException where
--   show (DuplicatePlayer p ps) =
--     "Player " ++ (userName p) ++ " is duplicated.\n"
--     ++ "Here are the current players in the game:\n"
--     ++ (show ps)
--   show (NotEnoughPlayers ps) =
--     "There are not enough players to play the game."
--     ++ "\nConsider adding players. Here are the "
--     ++ "current players:\n" ++ (show ps)
--   show (CoordinateOutOfBounds c l) =
--     "Coordinate entered is out of the bounds of the "
--     ++ "game: " ++ (show c) ++ ".\nChoose a "
--     ++ "coordinate within " ++ (showBounds l)
--   show (CoordinateOccupied c l owner attemptor) =
--     "Request by " ++ (userName attemptor)
--     ++ " to claim board spot " ++ (show $ toUserCoordinate (width l) c)
--     ++ " is not permitted.\nThis spot is already taken"
--     ++ " by " ++ (userName owner) ++ "."
--   show (InvalidDim d) =
--     "Unable to create board for dimension " ++ (show d)
--     ++ ".\nPick a dimension greater than 1."
--   show (MarkTooShort s) =
--     "Mark '" ++ s ++ "' is too short. Pick a mark exactly"
--     ++ " one character long."
--   show (NameTooShort u) =
--     "Name '" ++ u ++ "' is too short. Pick a name at least"
--     ++ " one letter long."
--   show (MarkTooLong m) =
--     "Mark '" ++ m ++ "' is too long. Pick a mark exactly"
--     ++ " one character long."
--   show (APINotFound s) =
--     "Could not find action: " ++ s ++ "."
--   show (StringParseFailed s) =
--     "Could not parse input: " ++ s ++ "."
--
