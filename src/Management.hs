{-|
Module      : Management
Description : Management's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Management
    ( initUniverse
    , joinTheGame
    , Universe (..)
    , module Field 
    ) where

import GHC.Generics
import qualified Data.HashMap as HM
import qualified Web.Spock as WS
import Data.Time
import Data.Text
import Data.Maybe ( fromMaybe )
import System.Random ( uniformR, mkStdGen, StdGen ) 
import Data.IORef
import Data.Aeson
import Field


        

type RoundsMap = HM.Map Integer Round                

data Universe = Universe {
                roundsMap :: RoundsMap,
                roundToCont :: RoundsMap, --Maybe Round,
                keyGen :: StdGen, 
                message :: String,
                errorInUniverse :: Maybe MyError
                } deriving (Eq, Show)

-- | Registration of 2nd player
initRoundCont :: String     -- ^ 2nd player's name
              -> Integer    -- ^ Round ID
              -> RoundsMap   -- ^ RoundsMap where the round has to be adjusted
              -> RoundsMap
initRoundCont name = HM.adjust f 
    where f r = r { player2 = Just name }
    
initID :: StdGen -> Round -> (StdGen, Round)
initID g r = (newG, newR)        
    where
        (n,newG) = uniformR (0,10^6) g
        newR = r { roundID = n }

initRound :: StdGen -> String -> RoundType -> (StdGen, Round)
initRound g name = initID g . initRound' name
                       
initUniverse :: Universe
initUniverse = Universe {
                roundsMap = HM.empty ,
                roundToCont = HM.empty, --Nothing,
                keyGen = mkStdGen 1375,
                message = "",
                errorInUniverse = Nothing
                }

-- | Allows joining the game: if there is one open awaiting, it's joined. Otherwise
--   a new one is created to wait for the next player.    
joinTheGame :: String -> Int -> Universe -> (Universe,Round)
joinTheGame name n u | HM.member n' rTCs                -- there is an initialized round waiting for 2nd player to continue
                        = (u { roundsMap = newRoundsMap
                             , roundToCont = HM.delete n' rTCs
                             } 
                            , rTC)
                     | otherwise                        -- there is no initialized round for n games
                        = (u {roundToCont = HM.insert n' newRound rTCs
                             ,keyGen = newKeyGen
                             }
                            , newRound)
    where
        rTCs = roundToCont u                          -- HashMap of Rounds
        rTC = (rTCs HM.! n') { player2 = Just name }  -- the round to continue initialization
        n' = fromIntegral n
        (newKeyGen,newRound) = initRound (keyGen u) name (NGames n)
        newRoundsMap = HM.insert (roundID rTC) rTC (roundsMap u)

-- | Progresses the Round gven by reference within the Universe by given Move.
--   In case of error (incorrect move, wrong key) provides with the error.
runTheGame :: Universe  -- ^ Initial state
            -> Integer  -- ^ key to the Round to be progressed
            -> Move     -- ^ the Move. Move can be incomplete, can be composed of sequential moves of opponents, 
                        --   the only requirement is that is composed of sequential sections, regardless the player.
            -> Universe -- ^ Output state
runTheGame u ix m = newUniverse
    where
        eitherG = case HM.lookup ix (roundsMap u) of          
                    Nothing -> Left $ WrongID "fatal error: incorrect game ID. Impossible to identify the game on server side."
                    Just ro -> validateMove (currentGame ro) m
        newUniverse = case eitherG of
          Left s -> u { errorInUniverse = Just s }
          Right g -> u { roundsMap = HM.adjust (\ro -> ro { currentGame = g} ) ix (roundsMap u) }
