module Game
    ( GameInteraction
    , GameState
    , Player(..)
    , Response
    , addPlayer
    , route
    , respond
    , setBoardSize
    , showBoard
    , startGame
    ) where

import Control.Monad            (join)
import Data.List                (intercalate, intersperse, uncons)
import Data.Maybe               (fromMaybe, fromJust)
import Text.Read                (readMaybe)
import Data.Monoid              ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V


-- | API layer type aliases.
type Request = String
type Param = String
type BoardSize = Int
type Response = String
type GameInteraction = (GameState, Response)
type PlayerResult = (Player, Result)


-- | Route any request.

-- In the future this parsing should be handled by
-- some more structured parser, such as Optparse-Applicative,
-- but it's not yet implemented.
--
-- Moreover, we could replace any signature whose parameters
-- include a GameInteraction and replace it with a State
-- monad (or transformer), but this likewise is not yet
-- implemented.
route :: GameInteraction -> Request -> GameInteraction
route i r = routeReq i (parseReqParams . sanitize $ r)

routeReq :: GameInteraction -> (Request, [Param]) -> GameInteraction
routeReq c ("pickSpot", ps)     = pickSpot ps c
routeReq c (r, _)               = requestNotFound c r

sanitize :: Request -> Request
sanitize = T.unpack . T.strip . T.pack

parseReqParams :: Request -> (Request, [Param])
parseReqParams r = fromMaybe ("", []) (uncons . words $ r)


-- | Unwrap the response from any game interaction.
respond :: GameInteraction -> Response
respond (_, s) = s

startGame :: GameInteraction
startGame = (Start, startMessage)

startMessage :: Response
startMessage = unlines $ [welcomeMessage, apiDescription, howToExit]

requestNotFound :: GameInteraction -> Request -> GameInteraction
requestNotFound (state, _) r = (state, "Request not found: " ++ r ++ ".")


-- |  A 'GameState' evolves as the game progresses.
--    A 'GameInteraction' is just a thin layer around a 'GameState'.
data GameState = Start
               | Turn Lattice [Player] GameState
               | Done Lattice [PlayerResult] GameState


-- | Display the state of the 'GameInteraction' to the user
--   without modifying it.
showBoard :: GameInteraction -> GameInteraction
showBoard (state, _) = (state, prettyLattice (getLattice state))

-- | Validate a new 'Player' and add to the 'GameState'.
addPlayer :: Player -> GameInteraction -> GameInteraction
addPlayer p (oldState, _) =
  let (u, m)      = (userName p, mark p)
      currPlayers = getPlayers oldState
      currMarks   = map mark currPlayers
      l           = getLattice oldState
      newL        = newLattice . width $ l
      newState    = if currPlayers == default2Players
                      then Turn newL [p] oldState
                      else Turn l (p:currPlayers) oldState
  in if m `elem` currMarks
       then (oldState, duplicatePlayer (u, m))
       else (newState, successAddPlayer p)


-- | Default size of the classic Tic Tac Toe board.
defaultSize :: Int
defaultSize = 3

getPlayers :: GameState -> [Player]
getPlayers Start         = default2Players
getPlayers (Turn _ ps _) = ps
getPlayers (Done _ rs _) = map fst rs

getBoardSize :: GameState -> Int
getBoardSize Start          = defaultSize
getBoardSize (Turn l _ _)   = width l
getBoardSize (Done l _ _)   = width l

getLattice :: GameState -> Lattice
getLattice Start          = newLattice $ getBoardSize Start
getLattice (Turn l _ _)   = l
getLattice (Done l _ _)   = l

-- | Rotate a list by the specified amount.
-- Use to manage whose turn it is.
rotate :: Int -> [a] -> [a]
rotate = drop <> take


setBoardSize :: BoardSize -> GameInteraction -> GameInteraction
-- setBoardSize (Turn, _) = (Prompt losehistory)
setBoardSize b (state, _) =
  let ps = getPlayers state
  in if b < 1
       then (state, dimTooSmall b)
       else (Turn (newLattice b) ps state, successSizeChange b)



-- | Pick a spot on the board for the player whose turn it is.
-- Input is parsed, validated, and the game will enter a new,
-- possibly terminal, state.
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
  | isWinningMove p spot l = winsResponse p spot curr
  | otherwise              = claimOkResponse p spot curr
  where l      = getLattice curr
        taker  = getTaker spot l
        p      = head . getPlayers $ curr

isDone :: GameState -> Bool
isDone (Done _ _ _) = True
isDone _            = False

isOutOfBounds :: LatticeCoordinate -> Lattice -> Bool
isOutOfBounds c l = let v   = toVCoordinate (width l) c
                        len = V.length . vec $ l
                    in 0 > v || v >= len

getTaker :: LatticeCoordinate -> Lattice -> Maybe Player
getTaker c l = join $ (vec l) V.!? (toVCoordinate (width l) c)

isWinningMove :: Player -> LatticeCoordinate -> Lattice -> Bool
isWinningMove p c l =
  let new = addTac p c l
      row = ownsRow p c new
      col = ownsColumn p c new
      ld  = ownsDescDiag p new
      rd  = ownsAscDiag p new
  in row || col || ld || rd


-- | Coordinates of the board
-- VCoord is a coordinate in the underlying vector.
-- UCoord is a "user coordinate".
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

-- | The board on which the game is played.
-- For a 2x2 board with 2 players (E and M) this might look like:
-- ---------
-- | E | M |
-- |---|---|
-- | E |   |
-- ---------
newtype Lattice = Lattice { vec :: V.Vector Tac } deriving (Eq, Show)

-- | Add a "tac" to the board for the given player at the
-- given coordinate.
-- The coordinate is presumed valid.
addTac :: Player -> LatticeCoordinate -> Lattice -> Lattice
addTac p c l = let tac = Just p
                   i   = toVCoordinate (width l) c
               in Lattice $ (vec l) V.// [(i, tac)]

-- | Get the width of the board.
-- E.g. for a (3 x 3) board, the width is 3.
width :: Lattice -> Int
width l = let area = (fromIntegral . V.length . vec $ l) :: Double
         in (fromIntegral . round . sqrt $ area) :: Int

newLattice :: Int -> Lattice
newLattice n = Lattice $ V.replicate (n ^ 2) Nothing

owns :: Player -> Tac -> Bool
owns p (Just q) = p == q
owns _ Nothing = False

ownsRow :: Player -> LatticeCoordinate -> Lattice -> Bool
ownsRow p c l =
  let w          = width l
      firstInRow = UserCoordinate . (,) 1 . snd . toUserCoordinate w $ c
      i          = toVCoordinate w firstInRow
      row        = V.slice i w (vec l)
  in V.all (owns p) row

ownsColumn :: Player -> LatticeCoordinate -> Lattice -> Bool
ownsColumn p c l =
  let w   = width l
      i   = toVCoordinate w c
      col = V.ifilter (\ix _ -> ix `mod` w == i `mod` w) $ vec l
  in V.all (owns p) col

ownsDescDiag :: Player -> Lattice -> Bool
ownsDescDiag p l =
  let w           = width l
      onDiag i _  = let (rx, cx) = toUserCoordinate w (VectorCoordinate i)
                    in rx == cx
      diag        = V.ifilter onDiag $ vec l
  in V.all (owns p) diag

ownsAscDiag :: Player -> Lattice -> Bool
ownsAscDiag p l =
  let w           = width l
      onDiag i _  = let (rx, cx) = toUserCoordinate w (VectorCoordinate i)
                    in rx + cx == w + 1
      diag        = V.ifilter onDiag $ vec l
  in V.all (owns p) diag

prettyLattice :: Lattice -> String
prettyLattice lattice =
  let v               = vec lattice
      side            = width lattice
      cap             = take (4 * side + 1) $ repeat '-'
      level l         = V.slice (l * side) side v
      tacMarks        = V.toList . fmap tacMark . level
      wrap l r center = l ++ center ++ r
      bookend         = wrap "| " " |"
      levelStr        = bookend . intercalate " | " . map (:[]) . tacMarks
      between         = ('|':) . concat . take side $ repeat "---|"
      allLevels       = map levelStr [0..(side-1)]
      uncapped        = intersperse between allLevels
  in unlines $ [cap] ++ uncapped ++ [cap]




-- | Populates the 'Lattice'.
type Tac = Maybe Player
type Mark = Char
type UserName = String

-- | Result of a whole game.
data Result = Win | Lose
  deriving (Eq, Show)

-- | A Tic Tac Toe player. Each player has a unique mark, which identifies their
-- 'Move's on the 'Lattice'.
data Player = Person UserName Mark
            -- | Computer UserName Mark Strategy
            deriving (Show)

instance Eq Player where
  p1 == p2 = mark p1 == mark p2

emptyMark :: Mark
emptyMark = ' '

-- | Gets the Mark at a 'Tac' or ' '
tacMark :: Tac -> Mark
tacMark = maybe emptyMark mark


default2Players :: [Player]
default2Players = [ Person "Player 1" '1'
                  , Person "Player 2" '2'
                  ]

-- data Strategy = RandomMove deriving (Eq, Show)

mark :: Player -> Char
mark (Person _ m) = m
-- mark (Computer _ m _) = m

userName :: Player -> String
userName (Person u _) = u
-- userName (Computer u _ _) = u

showResults :: [PlayerResult] -> String
showResults = unlines . map showResult

playerResults :: Player -> [Player] -> [PlayerResult]
playerResults winner ps = map result ps
  where result p
          | p == winner = (p, Win)
          | otherwise   = (p, Lose)



-- | API Messages
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

showResult :: PlayerResult -> String
showResult (p, Win)   = (userName p) ++ " won!"
showResult (p, Lose)  = (userName p) ++ " lost."

outOfBoundsResponse :: LatticeCoordinate
                    -> GameState
                    -> GameInteraction
outOfBoundsResponse c g = (g, r)
  where r = "Coordinate entered is out of the bounds of the game: "
            ++ (show c) ++ ".\nChoose a coordinate within "
            ++ (showBounds . getLattice $ g)

duplicatePlayer :: (UserName, Mark) -> Response
duplicatePlayer (u, m) = "Could not add player for username '" ++ u
                         ++ "' with mark '" ++ (m:"'.")
                         ++ "No two players in a game may use the same mark."

successAddPlayer :: Player -> Response
successAddPlayer p = "Successfully added player " ++ (userName p) ++ "."

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

takenResponse :: Player
              -> LatticeCoordinate
              -> GameState
              -> GameInteraction
takenResponse p c g = (g, r)
  where r = "Request to claim board spot "
            ++ (show $ toUserCoordinate (width . getLattice $ g) c)
            ++ " is not permitted.\nThis spot is already taken  by "
            ++ (userName p) ++ "."

alreadyDoneResponse :: GameState -> GameInteraction
alreadyDoneResponse s = (s, "This game is over. No more moves are allowed.")

winsResponse :: Player
             -> LatticeCoordinate
             -> GameState
             -> GameInteraction
winsResponse p c g =
  let l       = getLattice g
      ps      = getPlayers g
      newL    = addTac p c l
      results = playerResults p ps
      newG    = Done newL results g
      res     = unlines $ [ "Game over!"
                          , showResults results
                          , "Thanks for playing!"
                          ]
  in (newG, res)


claimOkResponse :: Player
                -> LatticeCoordinate
                -> GameState
                -> GameInteraction
claimOkResponse p c g =
  let newL = addTac p c (getLattice g)
      newP = rotate 1 (getPlayers g)
      newG = Turn newL newP g
  in (newG, "Player " ++ (userName p) ++ " successfully moved to "
            ++ (show $ toUserCoordinate (width newL) c) ++ ".")

showBounds :: Lattice -> String
showBounds l =
  let dimension = width l
  in "(1 <= number of columns <= " ++ (show dimension)
     ++ ", 1 <= number of rows <= " ++ (show dimension) ++ ")"
