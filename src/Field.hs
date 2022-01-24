{-|
Module      : Field
Description : Field's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
{-# LANGUAGE TemplateHaskell #-}

module Field
    ( Location
    , Section
    , Player (..)
    , Move
    , Grid
    , Game (..)
    , RoundType (..)
    , Round (..)
    , MyError (..)
    , initField
    , initFieldWithFrames
    , initGame
    , initGoals
    , initRound'
    , isPosEndOfMove
    , validateMove
    , currentGame
    , roundID
    , grid
    , sizeOfGame
    , currentPosition
    , newPosition
    , result
    , roundType
    , player1
    , player2
    , numberOfCurrentGame
    , iAm
    , currentPlayer
    , moveToCont
    , orderSection
    , isEndOfMove
    , goals
    , gameResult
    , isLastGameOfRound
    ) where

import           GHC.Generics
import          Control.Lens
import qualified Data.HashMap as HM
import qualified Data.Array as A
import           Data.Hashable
import           Data.Aeson
import           Data.Aeson.Types

-- | Location = Position = point on 2 dimentional discrete plane
type Location = (Int,Int) 

-- | Section is not a vector! I.e. (a,b) :: Section === (b,a) :: Section
type Section = (Location,Location)

-- | Complete set of graph's edges.
type Grid = HM.Map Section ()

-- | FieldWidth must be an even number >= 6. Field is a square
type FieldWidth = Int
-- | FieldLength must be an even number >= 6. Field is a square
type FieldLength = Int

-- | A move is a list of connected sections, possibly with loops. Ex [(a,b),(b,c),(c,a),(a,d)]
type Move = [Section]

data Player = Player1 | Player2
    deriving (Show, Eq, Ord, A.Ix, Generic, ToJSON, FromJSON)

playerSwap :: Player -> Player
playerSwap Player1 = Player2    
playerSwap Player2 = Player1

data MyError = IncorrectSection String    
            | BusySection String 
            | WrongID String
            deriving (Eq, Show)

theSize :: Int
theSize = 8            

-- | Goals represent all possible results of the game, i.e. for reaching certain location. All locations without rewards = 0.
--   Reward is always from Player1 point of view. Player2 has reward opposite to Player2. 
type Goals = A.Array (Location          -- ^ Where the reward is obtained
                     ,Player)           -- ^ Who has to reach location for the reward
                     Int                -- ^ Value of reward
    
instance (A.Ix ix, ToJSON ix, ToJSON v) => ToJSON (A.Array ix v) where
    toJSON = toJSON . A.assocs
instance (A.Ix ix, FromJSON ix, FromJSON v) => FromJSON (A.Array ix v) where
    parseJSON xs = do
                    ls <- parseJSON xs
                    let m = minimum $ map fst ls
                    let n = maximum $ map fst ls
                    return $ A.array (m,n) ls

instance (ToJSON k, ToJSON v) => ToJSON (HM.Map k v) where
    toJSON = toJSON . HM.toList
instance (Hashable k, Ord k, FromJSON k, FromJSON v) => FromJSON (HM.Map k v) where
    parseJSON xs = parseJSON xs >>= return . HM.fromList

-- | Scenario of a round
data RoundType =  NGames Int
                | BestOfN Int
                deriving (Eq, Generic, ToJSON, FromJSON)

instance Show RoundType where
    show (NGames 1) = "1 game"
    show (NGames n) = show n ++ " games"
    show (BestOfN n) = show "Best of " ++ show n

-- | Game represents whole information needed to play / perform next move
data Game = Game {
                _sizeOfGame :: Int,
                _grid :: Grid,
                _currentPosition :: Location,
                _newPosition :: Location,
                _moveToCont :: [Section],
                _currentPlayer :: Player,
                _iAm :: Player,
                _goals :: Goals,
                _gameResult :: Int -- ^ Game result from Player1 point of view. The game finishes when result /= 0
                } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Round is a Game wrapped in additional contex information. Set of games are performed within one Round.
data Round = Round {
               -- | State of the current game
               _currentGame :: Game,
               -- | First player's name
               _player1 :: Maybe String,
               -- | 2nd player's name
               _player2 :: Maybe String,
               -- | Current result of series of games - i.e. the round
               _result :: (Int,Int),
               -- | # of the current game needed in case game is planed for fix number of games
               _numberOfCurrentGame :: Int,
               -- | Different rounds' types can be imagined, like best of N, N games, ...
               _roundType :: RoundType,
               -- | The common ID for both players
               _roundID :: Integer
                } deriving (Eq, Show, Generic, ToJSON, FromJSON)
                
makeLenses 'Game
makeLenses 'Round

-- | New game initalization
initRound' :: String         -- ^ 1st player's name
            -> RoundType    -- ^ Type of game
            -> Round
initRound' name t = Round {
                        _currentGame = initGame theSize,
                        _player1 = Just name,
                        _player2 = Nothing,
                        _result = (0,0),
                        _numberOfCurrentGame = 1,
                        _roundType = t,
                        _roundID = 0
                        }

endOfRound :: Round -> Bool
endOfRound r = case r ^. roundType of  
                NGames n  -> n == one + two 
                BestOfN n -> round ((fromIntegral n+1) / 2) == max one two
    where
        (one,two) = r ^. result

roundSize :: RoundType -> Int
roundSize (NGames n) = n
roundSize (BestOfN n) = n

initGame :: Int -> Game
initGame s = Game {
                _sizeOfGame = s,
                _grid = initField s s,
                _currentPosition = (0,0),
                _newPosition = (0,0),
                _moveToCont = [],
                _currentPlayer = Player1,
                _iAm = Player1,
                _goals = initGoals s,
                _gameResult = 0
                } 

initField :: FieldLength -> FieldWidth -> Grid
initField len wid = buildField limits l w
    where
        w = wid `div` 2
        l = len `div` 2
        -- each location has to be within the field
        limits :: Location -> Location -> Bool
        limits (x,y) (x',y')= abs x' <= w && abs y' <= l -- to exclude sections being out of field
                            && (x /= x' || abs x < w) -- to exclude sides of the field
                            && (y /= y' || abs y < l) -- to exclude bottom and top ends of the field

-- to keep sections in order, they are sorted by x firstly then by y
orderSection :: Section -> Section
orderSection s@((x,y),(x',y'))  | x > x'            = ((x',y'),(x,y))
                                | x == x' && y > y' = ((x',y'),(x,y))
                                | otherwise         = s


buildField :: (Location -> Location -> Bool) -> FieldLength -> FieldWidth -> Grid
buildField limits l w = HM.fromList $ zip allSections (repeat ())
    where
        allLocations :: [Location]
        allLocations = [(x,y) | x <- [-w..w], y <- [-l..l] ]
        -- it is sufficient to take following 4 sections of each location to cover whole field: \|/_
        locationSections :: Location -> [Section]
        locationSections (x,y) = [((x,y),end) | end <- filter (limits (x,y)) [(x-1,y+1),(x,y+1),(x+1,y+1),(x+1,y)]]
        topGoal :: [Section]                                     
        topGoal = [((-1,l),(0,l)),
                   ((0,l),(1,l)),
                   ((0,l),(0,l+1)),
                   ((-1,l),(0,l+1)),
                   ((0,l+1),(1,l))]
        bottomGoal :: [Section]                                     
        bottomGoal = [((-1,-l),(0,-l-1)),
                    ((-1,-l),(0,-l)),
                    ((0,-l-1),(0,-l)),
                    ((0,-l),(1,-l)),
                    ((0,-l-1),(1,-l))]
        allSections :: [Section]
        allSections = map orderSection $ concatMap locationSections allLocations ++ topGoal ++ bottomGoal

initFieldWithFrames :: FieldLength -> FieldWidth -> Grid
initFieldWithFrames len wid = buildField limits l w
    where
        w = wid `div` 2
        l = len `div` 2
        -- each location has to be within the field
        limits :: Location -> Location -> Bool
        limits (x,y) (x',y')= abs x' <= w && abs y' <= l -- to exclude sections being out of field

-- | Puts values to certain locations. Goals are obvious - full point for moving current location to the goal.
--   Additionally there are 4 corners, player who moves current location to there looses half of points, the opponent gains the same value.
initGoals :: Int -> Goals
initGoals size = A.array (((-s,-s-1),Player1), ((s,s+1),Player2)) 
                         [(((x,y),who),0) | x <- [-s..s], y <- [-s-1..s+1], who <- [Player1, Player2]]
                 A.// [(((0,s+1),Player1),2)
                      ,(((0,s+1),Player2),2)
                      ,(((0,-s-1),Player1),-2)
                      ,(((0,-s-1),Player2),-2)
                      ,(((-s,-s),Player1),-1)
                      ,(((-s,-s),Player2),1)
                      ,(((-s,s),Player1),-1)
                      ,(((-s,s),Player2),1)
                      ,(((s,-s),Player1),-1)
                      ,(((s,-s),Player2),1)
                      ,(((s,s),Player1),-1)
                      ,(((s,s),Player2),1)
                      ]
    where s = size `div` 2         
            
validateMove :: Game -> Move -> Either MyError Game            
validateMove g = foldl validateSection (Right g)
    where
        validateSection :: Either MyError Game -> Section -> Either MyError Game
        validateSection (Left e) _ = Left e
                                               -- gameResult /= 0 => this game has been finished. Nothing to validate
        validateSection (Right g) s@(a,b) | g ^. gameResult /= 0 =
                                                        Right g 
                                            -- next section starts not in the place where it should have started => illegal move
                                          | from /= g ^. currentPosition &&
                                            to   /= g ^. currentPosition= 
                                                        Left $ IncorrectSection ("Incorrect section " ++ show s ++ 
                                                                " at current position" ++ show (g ^. currentPosition))
                                            -- illegal move
                                          | HM.lookup s (g ^. grid) == Nothing  = 
                                                        Left $ BusySection ("Section already busy " ++ show s)
                                          | otherwise = Right $ g & grid            .~ HM.delete s (g ^. grid)
                                                                  & currentPosition .~ to
                                                                  & currentPlayer   .~ newPlayer
                                                                  & newPosition     .~ to -- this line is possibly superflouous
                                                                  & gameResult      .~ (g ^. goals) A.! (to,g ^. currentPlayer)
                                                                  & moveToCont      .~ newMoveToCont
            where
                newPlayer   | isPosEndOfMove to g = playerSwap $ g ^. currentPlayer
                            | otherwise           = g ^. currentPlayer
                                -- moveToCont gets reseted during validation of 1st section
                newMoveToCont   | isPosStartOfMove g  = [s]
                                | otherwise           = g ^. moveToCont ++ [s]
                -- From and to have to be calculated as section is sorted by its ends, 
                -- so the first end isn't necessarily the "from"
                from    | a == g ^. currentPosition = a
                        | otherwise                 = b
                to      | a == g ^. currentPosition = b
                        | otherwise                 = a
                                                                                  
nextPlayer :: Game -> Game
nextPlayer g = g & currentPlayer .~ playerSwap (g ^. currentPlayer)

allSections :: Location -> [Section]
allSections (x,y) = [((a,b),(x,y)) | a <- [x-1..x], b <- [y-1..y+1], a /= x || b == y-1]
                 ++ [((x,y),(a,b)) | a <- [x..x+1], b <- [y-1..y+1], a /= x || b == y+1]

-- | Veryfies if the given location will be the end of the move.
--   Veryfication has to be done *before* the move uptades the game state.    
isPosEndOfMove :: Location -> Game -> Bool
isPosEndOfMove p gm = length (filter (`HM.member` (gm ^. grid)) (allSections p)) == 8 ||
                      (gm ^. goals) A.! (p,gm ^. currentPlayer) /= 0

-- | First section in the move has a little different treatment.
--   This helper funcion allows to recognize such a section.
isPosStartOfMove :: Game -> Bool
isPosStartOfMove gm = length (filter (`HM.member` (gm ^. grid)) (allSections $ gm ^. currentPosition)) == 7

-- | Veryfies if the current stat of the game points that player finished his move.
--   To be used on client side to decide if complete move can be now POSTed to the server.
isEndOfMove :: Game -> Bool
isEndOfMove g = length (filter (`HM.member` (g ^. grid)) (allSections $ g ^. currentPosition)) == 7 ||
                       (g ^. goals) A.! (g ^. currentPosition, g ^. currentPlayer) /= 0

isLastGameOfRound :: Round -> Bool 
isLastGameOfRound r = case r ^. roundType of  
                    NGames n  -> r ^. numberOfCurrentGame >= n && r ^. currentGame . gameResult /= 0
                    BestOfN n -> fromIntegral (uncurry  (+) (r ^. result)) > fromIntegral n / 2
