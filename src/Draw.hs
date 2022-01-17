{-|
Module      : Draw
Description : Draw's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
{-# LANGUAGE TemplateHaskell #-}

module Draw
    ( World (..)
    --, drawInversion 
    , drawWorld
    , initWorld
    , currentRound
    , factor
    , width
    , height
    , offset
    ) where

import qualified Data.HashMap as HM
import          Control.Lens
import Graphics.Gloss
import Field
import Management (initRound')

data World = World {
             _currentRound :: Round,
             _width :: Int, 
             _height :: Int, 
             _offset :: Int,
             _factor :: Float,
             _monitor :: String
} deriving Show

makeLenses 'World

initWorld :: Int -> World
initWorld s = World {
            _currentRound = initRound' "" (NGames 1),
            _width = 400,
            _height = 400,
            _offset = 100,
            _factor = 400 / (fromIntegral s + 2),
            _monitor = "?"
}

data Window = Window 
              { name :: String 
              , size :: (Int,Int)
              , location :: (Int,Int)}

drawSection :: Float -> Section -> Picture 
drawSection factor s@((x,y),(x',y')) = Line [(nx,ny),(nx',ny')]
    where
        nx = fromIntegral x * factor
        ny = fromIntegral y * factor
        nx' = fromIntegral x' * factor
        ny' = fromIntegral y' * factor

--drawInversion :: Float -> Location -> Grid -> Picture
--drawInversion factor loc field = pictures $ map (sectionColor . drawSection factor . fst) (HM.toList field)
--                                          ++ drawCurrentPosition loc factor

drawGrid :: World -> [Picture]
drawGrid w = map (sectionColor . drawSection (w ^. factor) . fst) (HM.toList $ HM.difference (initFieldWithFrames s s) g)
    where 
        g = w ^. currentRound . currentGame . grid -- $ currentGame $ currentRound w
        s = w ^. currentRound . currentGame . sizeOfGame -- $ currentGame $ currentRound w

sectionColor :: Picture -> Picture
sectionColor = color white

drawCurrentPosition :: World -> [Picture]
drawCurrentPosition w = map (color yellow)  [circle (0.15 * f), circle 1]
    where
        f = w ^. factor
        (x,y) = w ^. currentRound . currentGame . currentPosition -- $ currentGame $ currentRound w

drawNewPosition :: World -> [Picture]
drawNewPosition w = map (color yellow) [translate (f*fromIntegral x) (f*fromIntegral y) $ circle (0.2 *f)
                                       ,line section
                                       ] 
    where
        f = w ^. factor
        (x,y) = w ^. currentRound . currentGame . newPosition -- $ currentGame $ currentRound w
        (xc,yc) = w ^. currentRound . currentGame . currentPosition -- $ currentGame $ currentRound w 
        section = [(fromIntegral xc * f, fromIntegral yc * f),(fromIntegral x * f, fromIntegral y *f)]

drawMonitor :: World -> Picture 
drawMonitor w = color green $ scale 0.2 0.2 $ translate f f $ text $ w ^. monitor
    where
        f = w ^. factor

drawResult :: World -> Picture
drawResult w = color orange $ translate (f * t / 2) (f * t) $ scale 0.15 0.15 $ text prettyResult
    where
        f = w ^. factor
        t = fromIntegral (w ^. currentRound . currentGame . sizeOfGame) / 2 + 0.5
        (a,b) = w ^. currentRound . result
        prettyResult = show a ++ ":" ++ show b

drawRoundType :: World -> Picture
drawRoundType w = color orange $ translate (f * t / 2) (-f * t) $ scale 0.15 0.15 $ text prettyRound
    where
        f = w ^. factor
        t = fromIntegral (w ^. currentRound . currentGame . sizeOfGame) / 2 + 0.5
        prettyRound = show $ w ^. currentRound . roundType  

drawPlayers :: World -> [Picture]
drawPlayers w = map (color (greyN 0.7)) [drawP1,drawP2]
    where
        f = w ^. factor
        t = fromIntegral (w ^. currentRound . currentGame . sizeOfGame) / 2 + 0.5
        drawP1 = translate (-f * t) (-f * t) $ scale 0.15 0.15 $ text (pname $ w ^. currentRound . player1)
        drawP2 = translate (-f * t) (f * t) $ scale 0.15 0.15 $ text (pname $ w ^. currentRound . player2)
        pname p = case p of
                Just n -> n
                _      -> "Annonymous"

drawWorld :: World -> Picture 
drawWorld w = pictures $ drawRoundType w 
                        : drawResult w 
                        : drawMonitor w 
                        : drawPlayers w 
                        ++ drawCurrentPosition w 
                        ++ drawGrid w 
                        ++ drawNewPosition w
