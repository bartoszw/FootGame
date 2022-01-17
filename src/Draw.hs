{-|
Module      : Draw
Description : Draw's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Draw
    ( World (..)
    --, drawInversion 
    , drawWorld
    , initWorld
    ) where

import qualified Data.HashMap as HM
import Graphics.Gloss
import Field
import Management (initRound')

data World = World {
             currentRound :: Round,
             width :: Int, 
             height :: Int, 
             offset :: Int,
             factor :: Float,
             monitor :: String
} deriving Show

initWorld :: Int -> World
initWorld s = World {
            currentRound = initRound' "" (NGames 1),
            width = 400,
            height = 400,
            offset = 100,
            factor = 400 / (fromIntegral s + 2),
            monitor = "?"
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
drawGrid w = map (sectionColor . drawSection (factor w) . fst) (HM.toList $ HM.difference (initFieldWithFrames s s) g)
    where 
        g = grid $ currentGame $ currentRound w
        s = sizeOfGame $ currentGame $ currentRound w

sectionColor :: Picture -> Picture
sectionColor = color white

drawCurrentPosition :: World -> [Picture]
drawCurrentPosition w = map (color yellow)  [circle (0.15 * f), circle 1]
    where
        f = factor w
        (x,y) = currentPosition $ currentGame $ currentRound w

drawNewPosition :: World -> [Picture]
drawNewPosition w = map (color yellow) [translate (f*fromIntegral x) (f*fromIntegral y) $ circle (0.2 *f)
                                       ,line section
                                       ] 
    where
        f = factor w
        (x,y) = newPosition $ currentGame $ currentRound w
        (xc,yc) = currentPosition $ currentGame $ currentRound w 
        section = [(fromIntegral xc * f, fromIntegral yc * f),(fromIntegral x * f, fromIntegral y *f)]

drawMonitor :: World -> Picture 
drawMonitor w = color green $ scale 0.2 0.2 $ translate f f $ text $ monitor w        
    where
        f = factor w

drawResult :: World -> Picture
drawResult w = color orange $ translate (f * t / 2) (f * t) $ scale 0.15 0.15 $ text prettyResult
    where
        f = factor w
        t = fromIntegral (sizeOfGame $ currentGame $ currentRound w) / 2 + 0.5
        (a,b) = result $ currentRound w
        prettyResult = show a ++ ":" ++ show b

drawRoundType :: World -> Picture
drawRoundType w = color orange $ translate (f * t / 2) (-f * t) $ scale 0.15 0.15 $ text prettyRound
    where
        f = factor w
        t = fromIntegral (sizeOfGame $ currentGame $ currentRound w) / 2 + 0.5
        prettyRound = show $ roundType $ currentRound w

drawPlayers :: World -> [Picture]
drawPlayers w = map (color (greyN 0.7)) [drawP1,drawP2]
    where
        f = factor w
        t = fromIntegral (sizeOfGame $ currentGame $ currentRound w) / 2 + 0.5
        drawP1 = translate (-f * t) (-f * t) $ scale 0.15 0.15 $ text (pname $ player1 $ currentRound w)
        drawP2 = translate (-f * t) (f * t) $ scale 0.15 0.15 $ text (pname $ player2 $ currentRound w)
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
