module Main where

import Field
import qualified Data.HashMap as HM
import Graphics.Gloss
import Draw 
import Events


--width, height, offset :: Int
--width = 400
--height = 400
--offset = 100
theSize :: Int
theSize = 8

--factor :: Float
--factor = fromIntegral height / (fromIntegral theSize + 2)

window :: World -> Display
window w = InWindow "Foot" (width w, height w) (offset w, offset w)
     

background :: Color
background = black

main :: IO ()
--main = display window background $ drawInversion factor (currentPosition world) (HM.difference (initFieldWithFrames theSize theSize) (grid world))
--main = display (window w) background $ drawWorld w
main = playGame
--    where
--        w = initWorld theSize

--main = (print . fst . unzip . HM.toList . initField 6) 6

playGame :: IO ()
playGame = play (window w) background 0 w drawWorld handleEvent iterateWorld
    where
        iterateWorld _ w = w
        w = initWorld theSize