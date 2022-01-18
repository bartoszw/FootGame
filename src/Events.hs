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
import Graphics.Gloss
import Field
import Draw
import Graphics.Gloss.Interface.IO.Interact

-- | If case mouse points to an available section, the section is displayed as a hint
handleEvent :: Event -> World -> World 
handleEvent (EventMotion (x,y)) w 
            | itsMe     = w & monitor .~ show x' ++ "," ++ show y' ++ ":" ++ show curP
                            & currentRound . currentGame .newPosition .~ newP
            | otherwise = w
    where
        x' = round $ x / w ^. factor
        y' = round $ y / w ^. factor
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
            | newP == curP  = w
            | itsMe         = w & currentRound . currentGame .~ newG
            | otherwise     = w
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
        newG = case validateMove game [section] of          
          Left me -> undefined -- TODO display error message and get from server new state
          Right ga -> ga

handleEvent _ w = w
