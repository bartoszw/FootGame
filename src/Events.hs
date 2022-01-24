{-|
Module      : Events
Description : Events' main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Events
    ( handleEvent
    ) where

import          Control.Lens
import qualified Data.HashMap as HM
import           Data.Text (Text,pack, unpack)
import          Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL (toStrict,empty) 
import qualified Data.ByteString.Char8 as CS (pack) 
import qualified Data.Array as A  -- TODO dev only
import           Data.Aeson
import          Network.Wreq
import          Network.HTTP.Client  hiding (responseBody)
import          Graphics.Gloss 
import          Graphics.Gloss.Interface.IO.Interact
import Field
import Draw
import ClientServerTools
import Text.ParserCombinators.ReadP (between)


-- | Size of button
btNewGameX = 268
btNewGameY = 69
btNewGameTy = -1.3
btNewGameTx = 0

-- | If case mouse points to an available section, the section is displayed as a hint
handleEvent :: Event -> World -> IO World 
handleEvent (EventMotion (x,y)) w 
            | game ^. gameResult /= 0
                        = return $ w & overNewGameButton .~ overBt
            | itsMe     = return $ w & monitor .~ show x' ++ "," ++ show y' ++ ":" ++ show curP
                                     & currentRound . currentGame .newPosition .~ newP
            | otherwise = return w
    where
        f = w ^. factor
        t = fromIntegral (game ^. sizeOfGame) / 2 + 0.5
        x' = round $ x / w ^. factor
        y' = round $ y / w ^. factor
        overBt = x >= f * t * btNewGameTx - btNewGameX / 2 
              && x <= f * t * btNewGameTx + btNewGameX / 2 
              && y >= f * t * btNewGameTy - btNewGameY / 2 
              && y <= f * t * btNewGameTy + btNewGameY / 2 
        game = w ^. currentRound . currentGame
        curP = game ^. currentPosition
        g = game ^. grid
        itsMe = game ^. currentPlayer == game ^. iAm 
        newP | HM.member ((x',y'),curP) g ||
               HM.member (curP,(x',y')) g   = (x',y')
             | otherwise                    = curP

-- | Left mouse button click while mouse pointing to an available section, the section is validated and done.
--   In case move is finished it is so displayed.
handleEvent (EventKey (MouseButton LeftButton) Up _ (x,y) ) w 
            | newP == curP  = return w -- illegal move attempt
            | w ^. currentRound . player2 == Nothing 
                            = return $ w & monitor .~ "Waiting for 2nd player."
            | game ^. gameResult /= 0       -- This game is finished. No further move possible
                            = return w
            | itsMe         = case newG of
                                Right ga -> do
                                              print $ (ga ^. goals) A.! (ga ^. currentPosition, ga ^. currentPlayer)
                                              if isEndOfMove ga
                                              then do
                                                print $ "move " ++ show (ga ^. moveToCont)      -- TODO dev only
                                                postValidateMove (w & currentRound . currentGame .~ ga
                                                                    & currentRound . result . _1 +~ fst (newResult ga)
                                                                    & currentRound . result . _2 +~ snd (newResult ga)) 
                                                                (ga ^. moveToCont)
                                              else do
                                                print $ "section " ++ show section ++ " pos:" ++ show (ga ^. currentPosition) -- TODO dev only
                                                return $ w & currentRound . currentGame .~ ga
                                Left err -> return $ w & monitor .~ show err
            | otherwise     = return w
    where
        x' = round $ x / w ^. factor
        y' = round $ y / w ^. factor
        game = w ^. currentRound . currentGame
        curP = game ^. currentPosition
        g = game ^. grid
        itsMe = game ^. currentPlayer == game ^. iAm
        (section, newP) | HM.member ((x',y'),curP) g = (((x',y'),curP), (x',y'))
                        | HM.member (curP,(x',y')) g = ((curP,(x',y')), (x',y'))
                        | otherwise                  = ((curP,curP)   , curP)
        -- It's important to order the secion!
        newG = validateMove game [orderSection section] 
        newResult ga | ga ^. gameResult > 0 = (ga ^. gameResult, 0)
                     | otherwise            = (0, ga ^. gameResult)

handleEvent _ w = return w

-- | Sends the move to server side for validation.
--   Server responser OK / error, so the current state has to be maintained on client side.
postValidateMove :: World -> [Section] -> IO World
postValidateMove w m = do
      let url = "http://" ++ unpack (w ^. serverUrl) ++ ":" ++ show (w ^. serverPort) ++ "/move"
      let opts = defaults & param "move" .~ [encode2Text m] & param "round" .~ [encode2Text $ w ^. currentRound . roundID]
      print url                 -- TODO dev only
      print $ encode2Text m     -- TODO dev only
      rb <- postWith opts url BL.empty
      case asJSON rb of
        Right rr -> return $ w & monitor .~ rr ^. responseBody
        Left err -> print (show err) 
                    >> print (rb ^. responseBody)
                    >> (return $ w & monitor .~ show err)  -- TODO dev only print

