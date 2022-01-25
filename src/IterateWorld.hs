{-|
Module      : IterateWorld
Description : IterateWorld' main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module IterateWorld
    ( iterateWorld
    ) where

import          Control.Lens
import qualified Data.HashMap as HM
import           Data.Text (Text,pack, unpack)
import          Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL (toStrict,empty) 
import qualified Data.ByteString.Char8 as CS (pack) 
import           Data.Aeson
import          Network.Wreq
import          Network.HTTP.Client  hiding (responseBody)
import          Graphics.Gloss 
import          Graphics.Gloss.Interface.IO.Interact
import Field
import Draw
import ClientServerTools


-- | Being onTurn -> just tick
--   otherwise    -> get from server side current state - once every 5 ticks
iterateWorld :: Float -> World -> IO World 
iterateWorld freq w | me /= onTurn && w ^. ticks `mod` 2 == 0 = getTheMove w
                     -- waiting for 2nd player
                    | w ^. currentRound . player2 == Nothing
                        && w ^. ticks `mod` 2 == 0            = getTheMove w
                    | otherwise                               = return $ w & ticks +~ 1
    where
        game = w ^. currentRound . currentGame
        me = game ^. iAm
        onTurn = game ^. currentPlayer

-- | Sends the move to server side for validation.
--   Server responser OK / error, so the current state has to be maintained on client side.
getTheMove :: World -> IO World
getTheMove w = do
      let url = "http://" ++ unpack (w ^. serverUrl) ++ ":" ++ show (w ^. serverPort) ++ "/"
      let opts = defaults & param "round" .~ [encode2Text $ w ^. currentRound . roundID]
      print $ "getTheMove " ++ show (w ^. currentRound . roundID) -- TODO - only for dev
      rb <- getWith opts url
      case asJSON rb of
        Right rr -> return $ w & currentRound .~ rr ^. responseBody
                               & ticks +~ 1
        Left err -> print err >>                                    -- TODO only dev
                    print (rb ^. responseBody) >>
                    (return $ w & monitor .~ show (rb ^. responseBody)
                                & ticks +~ 1)

