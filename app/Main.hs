module Main where

import qualified Data.HashMap as HM
import Graphics.Gloss
import Data.IORef
import           Web.Spock
import Web.Spock.Config
import Draw 
import Management
import Server
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
main = launchWebServer
--    where
--        w = initWorld theSize

--main = (print . fst . unzip . HM.toList . initField 6) 6

displayWindow :: IO ()
displayWindow = display (window w) background $ drawWorld w
    where
        w = initWorld theSize


playGame :: IO ()
playGame = play (window w) background 0 w drawWorld handleEvent iterateWorld
    where
        iterateWorld _ w = w
        w = initWorld theSize

launchWebServer :: IO ()
launchWebServer = do
                    ref <- newIORef initUniverse
                    --pool <- runStdoutLoggingT $ createSqlitePool "mal.db" 5 
                    --runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
                    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState ref)
                    runSpock 8080 (spock spockCfg app)