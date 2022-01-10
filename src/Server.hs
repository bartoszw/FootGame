{-|
Module      : Server
Description : Server's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Server
    (MyAppState (..) 
    ,MySession (..)
    ,app
    ) where

import           GHC.Generics
import qualified Data.HashMap as HM
import           Web.Spock
import           Web.Spock.Action
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Data.Aeson hiding (json)
import           Data.IORef ( IORef, readIORef, writeIORef )
import           Control.Monad.IO.Class ( MonadIO(..) )
import System.Random ( uniformR )
import           Management

-- | Session is empty as we connect to right Round via RoundID
data MySession = EmptySession

-- | State of the application stores the Universe
newtype MyAppState = AppState (IORef Universe)

app :: SpockM () MySession MyAppState ()
app =
    do  middleware logStdoutDev
        get "home" getHome
        get "/" getHome
        post "join" postJoin
{-
        get "newpasswd" getHome
        get "hello" $ html helloHTML
        get ("hello" <//> var) $ \name1 -> do
              existingKeyMaybe <- runSQL $ fetchByName name1
              visitorNumber <- case existingKeyMaybe of
                  Nothing -> runSQL $ insertAndReturnKey name1
                  Just i -> return i
              text ("Hello " <> name1 <> ", you are visitor number " <> T.pack (show visitorNumber))
        post "login" getHome
        post "register" postRegister
        post "confirm" postConfirm
        post "/" $ do
          nameEntry <- decodeUsername <$> body
          sessId <- getSessionId 
          currentSessionRef <- readSession
          (AppState ref) <- getState 
          g <- liftIO $ readIORef ref
          let (rndNo,gNew) = randomR (0,9999) g
          let ud = UD nameEntry T.empty midday rndNo
          liftIO $ modifyIORef' currentSessionRef $ M.insert sessId ud
          liftIO $ writeIORef ref gNew
          redirect "home"    
        post "logout" $ do
          sessId <- getSessionId 
          currentSessionRef <- readSession
          liftIO $ modifyIORef' currentSessionRef $ M.delete sessId
          redirect "hello"       
          -}

-- | Route Home requirest parameter Round and provides with data of this round
getHome :: ActionCtxT () (WebStateM () MySession MyAppState) b
getHome =  do
            round <- param "round"
            case round of
              (Just r) -> getUniverse r 
              _        -> text "please provide with round id"

getUniverse :: (HasSpock (ActionCtxT ctx m), Control.Monad.IO.Class.MonadIO m,
 SpockState (ActionCtxT ctx m) ~ MyAppState) =>
 Integer -> ActionCtxT ctx m b
getUniverse r = do
                 (AppState ref) <- getState 
                 u <- liftIO $ readIORef ref
                 case HM.lookup r (roundsMap u) of          
                     Nothing -> text "incorrect game ID. Impossible to identify the game on server side."
                     Just ro -> json ro

postJoin :: ActionCtxT ctx (WebStateM () MySession MyAppState) a
postJoin = do
            (AppState ref) <- getState 
            u <- liftIO $ readIORef ref
            mName <- param "name"
            let nm = case mName of
                      Just n -> n
                      _      -> "Player " ++ show (fst $ uniformR (1,100) (keyGen u)::Int)
            mRound <- param "round"
            case mRound of
              Nothing -> text "missing wished number of games"
              Just n  -> do 
                      let (newU,newR) = joinTheGame nm n u
                      liftIO $ writeIORef ref newU
                      json newR
            