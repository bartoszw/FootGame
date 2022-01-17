{-|
Module      : Management
Description : Management's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
{-# LANGUAGE TemplateHaskell #-}

module Management
    ( initUniverse
    , joinTheGame
    , Universe (..)
    , module Field 
    , roundsMap
    , roundToCont
    , keyGen
    ) where

import          GHC.Generics
import          Control.Lens
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
                _roundsMap :: RoundsMap,
                _roundToCont :: RoundsMap, 
                _keyGen :: StdGen, 
                _message :: String,
                _errorInUniverse :: Maybe MyError
                } deriving (Eq, Show)

makeLenses 'Universe                

-- | Registration of 2nd player
initRoundCont :: String     -- ^ 2nd player's name
              -> Integer    -- ^ Round ID
              -> RoundsMap   -- ^ RoundsMap where the round has to be adjusted
              -> RoundsMap
initRoundCont name = HM.adjust f 
    where f r = r { _player2 = Just name }
    
initID :: StdGen -> Round -> (StdGen, Round)
initID g r = (newG, newR)        
    where
        (n,newG) = uniformR (0,10^6) g
        newR = r { _roundID = n }

initRound :: StdGen -> String -> RoundType -> (StdGen, Round)
initRound g name = initID g . initRound' name
                       
initUniverse :: StdGen -> Universe
initUniverse g = Universe {
                _roundsMap = HM.empty ,
                _roundToCont = HM.empty, --Nothing,
                _keyGen = g,
                _message = "",
                _errorInUniverse = Nothing
                }

-- | Allows joining the game: if there is one open awaiting, it's joined. Otherwise
--   a new one is created to wait for the next player.    
joinTheGame :: String -> Int -> Universe -> (Universe,Round)
joinTheGame name n u | HM.member n' rTCs                -- there is an initialized round waiting for 2nd player to continue
                        = (u { _roundsMap = newRoundsMap
                             , _roundToCont = HM.delete n' rTCs
                             } 
                            , rTC)
                     | otherwise                        -- there is no initialized round for n games
                        = (u {_roundToCont = HM.insert n' newRound rTCs
                             ,_keyGen = newKeyGen
                             }
                            , newRound)
    where
        rTCs = u ^. roundToCont                           -- HashMap of Rounds
        rTC = (rTCs HM.! n') { _player2 = Just name }  -- the round to continue initialization
        n' = fromIntegral n
        (newKeyGen,newRound) = initRound (u ^. keyGen) name (NGames n)
        newRoundsMap = HM.insert (rTC ^. roundID) rTC (u ^. roundsMap)

-- | Progresses the Round gven by reference within the Universe by given Move.
--   In case of error (incorrect move, wrong key) provides with the error.
runTheGame :: Universe  -- ^ Initial state
            -> Integer  -- ^ key to the Round to be progressed
            -> Move     -- ^ the Move. Move can be incomplete, can be composed of sequential moves of opponents, 
                        --   the only requirement is that is composed of sequential sections, regardless the player.
            -> Universe -- ^ Output state
runTheGame u ix m = newUniverse
    where
        eitherG = case HM.lookup ix (u ^. roundsMap) of          
                    Nothing -> Left $ WrongID "fatal error: incorrect game ID. Impossible to identify the game on server side."
                    Just ro -> validateMove (ro ^. currentGame) m
        newUniverse = case eitherG of
          Left s -> u { _errorInUniverse = Just s }
          Right g -> u { _roundsMap = HM.adjust (\ro -> ro { _currentGame = g} ) ix (u ^. roundsMap) }
