{-|
Module      : Management
Description : Management's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Management
    ( 
    ) where

import qualified Data.HashMap as HM
import qualified Web.Spock as WS
import Field

data Universe = Universe {
                currentGame :: Game,
                player1 :: Maybe String,
                player2 :: Maybe String,
                result :: (Int,Int),
                numberOfGames :: Int,
                numberOfCurrentGame :: Int,
                sessionID :: WS.SessionId
                }