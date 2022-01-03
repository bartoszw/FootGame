{-|
Module      : Field
Description : Field's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Field
    ( Location
    , Section
    , Grid
    , Game (..)
    , initField
    , initFieldWithFrames
    , initGame
    ) where

import qualified Data.HashMap as HM
import qualified Data.Array as A

type Location = (Int,Int)
-- | Section is not a vector! I.e. (a,b) :: Section === (b,a) :: Section
type Section = (Location,Location)
type Grid = HM.Map Section ()
-- | FieldWidth must be an even number >= 6. Field is a square
type FieldWidth = Int
-- | FieldLength must be an even number >= 6. Field is a square
type FieldLength = Int
type Move = [Section]
data Player = Player1 | Player2
    deriving (Show, Eq, Ord, A.Ix)
-- | Goals represent all possible results of the game, i.e. for reaching certain location. All locations without rewards = 0.
--   Reward is always from Player1 point of view. Player2 has reward opposite to Player2. 
type Goals = A.Array (Location          -- ^ Where the reward is obtained
                     ,Player)           -- ^ Who has to reach location for the reward
                     Int                -- ^ Value of reward

-- | Word represents whole information needed to draw current state of the game
data Game = Game {
                sizeOfGame :: Int,
                grid :: Grid,
                currentPosition :: Location,
                newPosition :: Location,
                currentPlayer :: Player,
                goals :: Goals,
                result :: Int -- ^ Game result from Player1 point of view. The game finishes when result /= 0
}

initGame :: Int -> Game
initGame s = Game {
                sizeOfGame = s,
                grid = initField s s,
                currentPosition = (0,0),
                newPosition = (0,0),
                currentPlayer = Player1,
                goals = initGoals s,
                result = 0
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


buildField :: (Location -> Location -> Bool) -> FieldLength -> FieldWidth -> Grid
buildField limits l w = HM.fromList $ zip allSections (repeat ())
    where
        allLocations :: [Location]
        allLocations = [(x,y) | x <- [-w..w], y <- [-l..l] ]
        -- it is sufficient to take following 4 sections of each location to cover whole field: \|/_
        locationSections :: Location -> [Section]
        locationSections (x,y) = [((x,y),end) | end <- filter (limits (x,y)) [(x-1,y+1),(x,y+1),(x+1,y+1),(x+1,y)]]
        -- to keep sections in order, they are sorted by x firstly then by y
        ordSection :: Section -> Section
        ordSection s@((x,y),(x',y')) | x > x'            = ((x',y'),(x,y))
                                     | x == x' && y > y' = ((x',y'),(x,y))
                                     | otherwise         = s
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
        allSections = map ordSection $ concatMap locationSections allLocations ++ topGoal ++ bottomGoal

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
            
validateMove :: Game -> Move -> Either String Game            
validateMove w m = foldl validateSection (Right w) m
    where
        validateSection :: Either String Game -> Section -> Either String Game
        validateSection (Left e) _ = Left e
        validateSection (Right w) s@(from,to) | result w /= 0 =
                                                        Right w 
                                              | from /= currentPosition w = 
                                                        Left ("Incorrect section " ++ show s ++ 
                                                              " at current position" ++ show (currentPosition w))
                                              | HM.lookup s (grid w) == Nothing  = 
                                                        Left ("Section already busy " ++ show s)
                                              | otherwise =
                                                        Right w {grid = HM.delete s (grid w)
                                                                ,currentPosition = to
                                                                ,newPosition = to
                                                                ,result = goals w A.! (to,currentPlayer w)}
                                                                                  
