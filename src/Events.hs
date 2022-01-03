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

import qualified Data.HashMap as HM
import Graphics.Gloss
import Field
import Draw
import Graphics.Gloss.Interface.IO.Interact

handleEvent :: Event -> World -> World 
handleEvent (EventMotion (x,y)) w = w {monitor = show x' ++ "," ++ show y' ++ ":" ++ show curP
                                      ,game = (game w) {newPosition = newP}
                                      }
    where
        x' = round $ x / factor w
        y' = round $ y / factor w
        curP = currentPosition $ game w
        g = grid $ game w
        newP | HM.member ((x',y'),curP) g ||
               HM.member (curP,(x',y')) g = (x',y')
             | otherwise             = currentPosition $ game w
               

handleEvent _ w = w