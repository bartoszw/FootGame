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
import           Control.Lens
import qualified Data.HashMap as HM
import           Web.Spock
import           Web.Spock.Action
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Data.Aeson hiding (json)
import qualified Data.Text as T
import           Data.IORef ( IORef, readIORef, writeIORef )
import           Control.Monad.IO.Class ( MonadIO(..) )
import           System.Random ( uniformR )
import           System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy.Char8 as CS
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
        post "move" postValidateMove
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

-- | Route Home requires parameter Round and provides with data of this round
getHome :: ActionCtxT () (WebStateM () MySession MyAppState) b
getHome =  do
            (AppState ref) <- getState 
            u <- liftIO $ readIORef ref
            iround <- param "round"
            case iround of
              Nothing -> getListOfActiveRounds
              Just ir -> case getRound u ir of
                Left txt     -> text txt
                Right (ro,p) -> json $ ro & currentGame . iAm .~ p      -- iAm is set in accordance to the player who sent the request
                                          & roundID .~ ir               -- roundID set separately for 1st and 2nd player

{-
agetRound :: (HasSpock (ActionCtxT ctx m), Control.Monad.IO.Class.MonadIO m,
 SpockState (ActionCtxT ctx m) ~ MyAppState) =>
 Integer -> ActionCtxT ctx m b
agetRound r = do
                 case HM.lookup r (u ^. idAccess2nd) of
                    Just idRo -> getRound' idRo u Player2 -- ID of 2nd player
                    Nothing   -> getRound' r u Player1    -- ID of 1st player == Round ID
    where getRound' r' u p =
                 case HM.lookup r' (u ^. roundsMap) of          
                     Just ro -> json $ ro & currentGame . iAm .~ p      -- iAm is set in accordance to the player who sent the request
                     Nothing -> case HM.lookup r' (u ^. roundToCont) of   -- TODO: temporary solution
                                  Just r2 -> json r2
                                  _       -> text ("incorrect game ID (" 
                                                  <> pack (show r) 
                                                  <> "). Impossible to identify the game on server side.")
-}
getRound :: Universe -> Integer -> Either T.Text (Round, Player)
getRound u r = do
                 case HM.lookup r (u ^. idAccess2nd) of
                    Just idRo -> getRound' idRo u Player2 -- ID of 2nd player
                    Nothing   -> getRound' r u Player1    -- ID of 1st player == Round ID
    where getRound' r' u p =
                 case HM.lookup r' (u ^. roundsMap) of          
                     Just ro -> Right (ro,p)
                     Nothing -> Left $ T.pack "incorrect game ID (" 
                                    <> T.pack (show r')
                                    <> "). Impossible to identify the game on server side."


getListOfActiveRounds :: (HasSpock (ActionCtxT ctx m), Control.Monad.IO.Class.MonadIO m,
 SpockState (ActionCtxT ctx m) ~ MyAppState) =>
 ActionCtxT ctx m b
getListOfActiveRounds = do
                 (AppState ref) <- getState 
                 u <- liftIO $ readIORef ref
                 json (HM.keys (u ^. roundsMap) , HM.assocs (u ^. idAccess2nd) , HM.keys (u ^. roundToCont))


postJoin :: ActionCtxT ctx (WebStateM () MySession MyAppState) a
postJoin = do
            (AppState ref) <- getState 
            u <- liftIO $ readIORef ref
            mName <- param "name"
            let nm = case mName of
                      Just n -> n
                      _      -> "Player " ++ show (fst $ uniformR (1,100) (u ^. keyGen)::Int)
            mRound <- param "round"
            case mRound of
              Nothing -> text "missing wished number of games"
              Just n  -> do 
                      let (newU,newR) = joinTheGame nm n u
                      liftIO $ writeIORef ref newU
                      json newR

postValidateMove :: ActionCtxT ctx (WebStateM () MySession MyAppState) a
postValidateMove = do
            (AppState ref) <- getState 
            u <- liftIO $ readIORef ref
            iround <- param "round"
            moveJ <- param "move"
            case (iround,moveJ) of
              (Just ir, Just move) -> 
                case eitherDecode (CS.pack move) of
                  Left s -> text "syntax issue: impossible to decode the move"
                  Right m -> do
                    let newU = runTheGame u ir m
                    liftIO $ writeIORef ref newU
                    let msg = case getTheGame ir newU of
                          Just round -> show (round ^. currentGame . currentPosition) ++ 
                                        show (round ^. currentGame . moveToCont)
                          Nothing    -> "Nothing"
                    case newU ^. errorInUniverse of
                      Nothing  -> json $"OK " ++ msg 
                      Just err -> json $ show err ++ msg
              _ -> text "missing game reference or move to proceed"

              

            