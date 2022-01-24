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
    , idAccess2nd
    , runTheGame
    , errorInUniverse
    , getTheGame
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

-- | Unique ID of a round maped to the round data
type RoundsMap = HM.Map Integer Round                

-- | 2nd player unique ID maped to Round's unique ID.
--   Both players have to have personal access keys to the same round. 1st player's personal access key is the same time the 
--   unique ID of the Round. 2nd player's personal access key first maps Round's unique ID.
type IdMap = HM.Map Integer Integer

-- | The whole universe of all Rounds on server side with context data
data Universe = Universe {
                _roundsMap :: RoundsMap,
                _idAccess2nd :: IdMap,
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
    where f r = r & player2 ?~ name
                  & currentGame . iAm .~ Player2
    
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
                _idAccess2nd = HM.empty,
                _roundToCont = HM.empty, --Nothing,
                _keyGen = g,
                _message = "",
                _errorInUniverse = Nothing
                }

-- | Allows joining the game: if there is one open awaiting, it's joined. Otherwise
--   a new one is created to wait for the next player.    
joinTheGame :: String -> Int -> Universe 
            -> (Universe,Round)     -- ^ Universe to be stored on server side, Round to be passed to the client
joinTheGame name n u | HM.member n' rTCs                -- there is an initialized round waiting for 2nd player to continue
                        = ( u & roundsMap   .~ newRoundsMap           
                              & idAccess2nd .~ newAccess2nd
                              & roundToCont .~ HM.delete n' rTCs   
                              & keyGen      .~ newkeyGen'                           
                            , rTC2nd)
                     | otherwise                        -- there is no initialized round for n games
                        = (u & roundToCont  .~ HM.insert n' newRound rTCs
                             & keyGen       .~ newKeyGen
                            , newRound)
    where
        rTCs = u ^. roundToCont                             -- HashMap of Rounds
        rTC = (rTCs HM.! n') & player2 ?~ name              -- the round to continue initialization
        rTC2nd = rTC & roundID .~ id'                -- 2nd player gets own game ID
                     & currentGame . iAm .~ Player2  -- 2nd player has to know who he is
        n' = fromIntegral n
        (newKeyGen,newRound) = initRound (u ^. keyGen) name (NGames n)
        newRoundsMap = HM.insert (rTC ^. roundID) rTC (u ^. roundsMap)    -- 2nd player
        (id',newkeyGen') = uniformR (0,10^6::Integer) (u ^. keyGen)       -- 2nd player's id generation
        newAccess2nd = HM.insert id' (rTC ^. roundID) (u ^. idAccess2nd)  -- 2nd player's id storage

-- | Progresses the Round given by reference within the Universe by given Move.
--   In case of error (incorrect move, wrong key) provides with the error.
runTheGame :: Universe  -- ^ Initial state
            -> Integer  -- ^ key to the Round (of 1st or 2nd player) to be progressed
            -> Move     -- ^ the Move. Move can be incomplete, can be composed of sequential moves of opponents, 
                        --   the only requirement is that is composed of sequential sections, regardless the player.
            -> Universe -- ^ Output state
runTheGame u ix m = newUniverse
    where
        -- check if RoundId exists
        eitherG i = case HM.lookup i (u ^. roundsMap) of          
                    Nothing -> Left $ WrongID "fatal error: incorrect game ID. Impossible to identify the game on server side."
                    Just ro -> validateMove (ro ^. currentGame) m
        -- 1st check if the move belongs to the 2nd player
        maybe2ndPlayer = case HM.lookup ix (u ^. idAccess2nd) of          
                    Nothing -> (eitherG ix, ix)        -- If not, then perhaps to the 1st one
                    Just rIx -> (eitherG rIx, rIx)     -- If yes, then look up using roundID
        newUniverse = case maybe2ndPlayer of
          (Left s, _)  -> u & errorInUniverse ?~ s
          (Right g, i) -> u & roundsMap .~ HM.adjust (\ro -> ro & currentGame .~ g
                                                                & result . _1 +~ fst (newResult g)     -- game result has to propagate 
                                                                & result . _2 +~ snd (newResult g))    -- to round level
                                                    i (u ^. roundsMap)
            where
                newResult ga | ga ^. gameResult > 0 = (ga ^. gameResult, 0)
                             | otherwise            = (0, ga ^. gameResult)

getTheGame :: Integer -> Universe -> Maybe Round
getTheGame ix u = case HM.lookup ix (u ^. idAccess2nd) of   -- check if ix exists as 2nd player's id
                    Nothing -> maybeG ix                    -- If not, then perhaps it's the 1st one
                    Just rIx -> maybeG rIx                  -- If yes, then look up using roundID of ix
    where                        
        maybeG i = HM.lookup i (u ^. roundsMap)
