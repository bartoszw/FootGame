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

handleEvent :: Event -> World -> World 
handleEvent (EventMotion (x,y)) w = w {_monitor = show x' ++ "," ++ show y' ++ ":" ++ show curP
                                      ,_currentRound = (w ^. currentRound) {
                                                        _currentGame = (w  ^. currentRound . currentGame) {_newPosition = newP}
                                                        }
                                        }
    where
        x' = round $ x / w ^. factor
        y' = round $ y / w ^. factor
        curP = w ^. currentRound . currentGame . currentPosition
        g = w ^. currentRound . currentGame . grid
        newP | HM.member ((x',y'),curP) g ||
               HM.member (curP,(x',y')) g = (x',y')
             | otherwise             = w ^. currentRound . currentGame . currentPosition
               

handleEvent _ w = w
