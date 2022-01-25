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
    , monitor
    , serverUrl
    , serverPort
    , ticks
    , overNewGameButton
    ) where

import           GHC.Generics
import qualified Data.HashMap as HM
import          Control.Lens
import qualified Data.Text as T
import          Graphics.Gloss
import           Data.Aeson
--import           Data.Aeson.Types
import          Field

data World = World {
             _currentRound :: Round,
             _width :: Int, 
             _height :: Int, 
             _offset :: Int,
             _factor :: Float,
             _monitor :: String,
             _ticks :: Int, -- TODO: for monitoring only
             _serverUrl :: T.Text,
             _serverPort :: Int,
             _overNewGameButton :: Bool
} deriving (Show, Generic, ToJSON, FromJSON)

makeLenses 'World

initWorld :: Int -> World
initWorld s = World {
            _currentRound       = initRound' "" (NGames 1),
            _width              = 400,
            _height             = 600,
            _offset             = 100,
            _factor             = 400 / (fromIntegral s + 2),
            _monitor            = "?",
            _ticks              = 0,
            _serverUrl          = "localhost",
            _serverPort         = 8080,
            _overNewGameButton  = False
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


colorSection :: Picture -> Picture
colorSection = color white
colorCurPosition = color yellow
colorNewPosition = color yellow
colorTicks = color chartreuse
colorMonitor = color green
colorResult = color orange
colorTheEnd = color (light red)
colorGameFinished = color (light $ light red)
colorRoundType = color orange

startNewGameButton :: IO Picture
startNewGameButton = loadBMP "./assets/images/button_start-new-game.bmp"
startNewGameButton2 :: IO Picture
startNewGameButton2 = loadBMP "./assets/images/button_start-new-game2.bmp"



scaleP1 :: Player -> Float
scaleP1 Player1 = 0.18          -- I'm Player1 -> bigger font
scaleP1 Player2 = 0.12
scaleP2 Player1 = 0.12
scaleP2 Player2 = 0.18          -- I'm Player2 -> bigger font
colorP1 Player1 = yellow        -- Player1 onTurn is yellow
colorP1 Player2 = greyN 0.7
colorP2 Player2 = yellow        -- Player2 onTurn is yellow
colorP2 Player1 = greyN 0.7

pname :: Maybe String -> String
pname p = case p of
            Just n -> n
            _      -> "Annonymous"

drawWorld :: World -> IO Picture 
drawWorld w = drawButtonNewGame >>=
                \p -> return $ pictures (p 
                                        : drawRoundType
                                        : drawGameFinished
                                        : drawTheEnd
                                        : drawResult
                                        : drawMonitor
                                        : drawTicks
                                        : drawPlayer1
                                        : drawPlayer2
                                        : drawCurrentPosition
                                        ++ drawGrid
                                        ++ drawNewPosition)

    where
        drawGrid :: [Picture]
        drawGrid = map (colorSection . drawSection (w ^. factor) . fst) (HM.toList $ HM.difference (initFieldWithFrames s s) g)

        game = w ^. currentRound . currentGame
        g = game ^. grid 
        s = game ^. sizeOfGame 

        drawCurrentPosition :: [Picture]
        drawCurrentPosition = map colorCurPosition [circle (0.15 * f), circle 1]

        f = w ^. factor
        (x,y) = game ^. currentPosition

        drawNewPosition :: [Picture]
        drawNewPosition = map colorNewPosition [translate (f*fromIntegral xc) (f*fromIntegral yc) $ circle (0.2 *f)
                                       ,line section
                                       ] 
        (xc,yc) = game ^. newPosition 
        section = [(fromIntegral x * f, fromIntegral y * f),(fromIntegral xc * f, fromIntegral yc *f)]

        me = game ^. iAm
        onTurn = game ^. currentPlayer
        t = fromIntegral (game ^. sizeOfGame) / 2 + 0.5
        drawPlayer1 = color (colorP1 onTurn) $ translate (-f * t) (-f * t) 
                                            $ scale (scaleP1 me) (scaleP1 me) 
                                            $ text (pname $ w ^. currentRound . player1)
        drawPlayer2 = color (colorP2 onTurn) $ translate (-f * t) (f * t) 
                                            $ scale (scaleP2 me) (scaleP2 me) 
                                            $ text (pname $ w ^. currentRound . player2)

        drawTicks :: Picture 
        drawTicks = colorTicks $ translateNx 0.5 $ textMedium (show $ w ^. ticks)

        drawMonitor :: Picture 
        drawMonitor = colorMonitor $ translateEx (-1.1) $ textSmall $ w ^. monitor

        drawResult :: Picture
        drawResult = colorResult $ translateNx 0.75 $ textMedium prettyResult
        (a,b) = w ^. currentRound . result
        prettyResult = show a ++ ":" ++ show b

        drawTheEnd :: Picture 
        drawTheEnd  | w ^. currentRound . player2 == Nothing
                = colorTheEnd  $ translateEx 0 $ textBig "Waiting for opponent"
                    | isLastGameOfRound (w ^. currentRound)
                = colorTheEnd $ translatexx (-0.5) (-0.1) $ textBig "The End"
                    | otherwise 
                = Blank

        drawGameFinished :: Picture 
        drawGameFinished    | w ^. currentRound . currentGame . gameResult /= 0
                    = colorGameFinished $ translatexx (-0.8) 0.1 $ textBig "Game Finished"
                            | otherwise 
                    = Blank

        drawButtonNewGame :: IO Picture
        drawButtonNewGame  | not (isLastGameOfRound (w ^. currentRound))            -- when this is not the last game of round
                            && w ^. currentRound . currentGame . gameResult /= 0    -- when the game finished
                            && me == onTurn                                         -- when I'm on move (just one player gets the button)
                    =   (if w ^. overNewGameButton
                        then startNewGameButton2
                        else startNewGameButton) <&> translatexx 0 (-1.3)
                            | otherwise
                    = return Blank

        drawRoundType :: Picture
        drawRoundType = colorRoundType  $ translateSx 0.5 $ textMedium prettyRound
        prettyRound = show $ w ^. currentRound . roundType  

        translateES = translate (-f * t) (-f * t) 
        translateEN = translate (-f * t) (f * t) 
        translateNx x = translate (f * t * x) (f * t)
        translateSx x = translate (f * t * x) (-f * t)
        translateEx y = translate (-f * t) (f * t * y)
        translatexx x y =  translate (f * t * x) (f * t * y)

        textBig = scale 0.3 0.3 . text
        textMedium = scale 0.15 0.15 . text
        textSmall = scale 0.1 0.1 . text