{-|
Module      : FootGame
Desription  : Executable for the Foot server
Copyright   : (c) 2022 Bartosz Wójcik
License     : BSD-3-Clause (see the LICENSE file)
-}
module Main where

import           System.Random
import           Data.Time.Clock.POSIX
import           Control.Lens
import qualified Data.HashMap as HM
import Graphics.Gloss
import Data.IORef
import           Web.Spock
import Web.Spock.Config
import Draw 
import Management
import Server
import Events


--theSize :: Int
--theSize = 8

--window :: World -> Display
--window w = InWindow "Foot" (w ^. width, w ^. height) (w ^. offset, w ^. offset)
     

--background :: Color
--background = black

main :: IO ()
--main = display window background $ drawInversion factor (currentPosition world) (HM.difference (initFieldWithFrames theSize theSize) (grid world))
--main = display (window w) background $ drawWorld w
main = launchWebServer
--    where
--        w = initWorld theSize

--main = (print . fst . unzip . HM.toList . initField 6) 6

--displayWindow :: IO ()
--displayWindow = display (window w) background $ drawWorld w
--    where
--        w = initWorld theSize


--playGame :: IO ()
--playGame = play (window w) background 0 w drawWorld handleEvent iterateWorld
--    where
--        iterateWorld _ w = w
--        w = initWorld theSize

launchWebServer :: IO ()
launchWebServer = do
                    -- Below line can be replaced by initStdGen >>= newIORef . initUniverse once random can be used in version >= 1.2.1 
                    ref <- (round <$> getPOSIXTime) >>= newIORef . initUniverse . mkStdGen 
                    --pool <- runStdoutLoggingT $ createSqlitePool "mal.db" 5 
                    --runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
                    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (AppState ref)
                    runSpock 8080 (spock spockCfg app)