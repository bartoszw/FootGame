{-|
Module      : FootPlayer
Desription  : Executable for the Foot player
Copyright   : (c) 2022 Bartosz Wójcik
License     : BSD-3-Clause (see the LICENSE file)
-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.IO ( stderr, hPutStr )
import          Control.Lens
import          Data.Text (Text,pack, unpack)
import          Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL (toStrict,empty) 
import qualified Data.ByteString.Char8 as CS (pack) 
import          Data.Maybe

import          Network.Wreq
import          Network.HTTP.Client  hiding (responseBody)
import qualified Control.Exception as E
import          Monomer
import qualified Graphics.Gloss as G
import          Management
import          Events
import qualified Draw


data AppModel = AppModel {
  _serverUrl :: Text,
  _myName :: Text,
  _numberOfGames :: Int,
  _roundId :: Int,
  _roundContent :: Maybe Round,
  _gameInRun :: Bool,
  _sampleText :: Text,
  _showPicker :: Bool,
  _fontName :: Font,
  _fontSize :: Double,
  _fontColor :: Color
} deriving (Eq)

data AppEvent
  = AppInit
  | AppStart
  | AppGetRound
  | AppSetRound Round
  | AppGetListOfRounds
  | AppSetListOfRounds Text
  | AppSetError Text
  | AppOnLeaveNbr
  deriving (Eq)

makeLenses 'AppModel

-- | ===================================  buildUI  ==================================================================
buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      hstack [
        inputFieldText "Your name",
        box (textField myName) `styleBasic` [paddingH 10]
      ] `styleBasic` [paddingV 10],

      hstack [
        inputFieldText "How many games you wish to play in this round",
        box (numericField_ numberOfGames [minValue 1, maxValue 10, onBlur (\_ -> AppOnLeaveNbr)]) 
          `styleBasic` [paddingH 10],
        label_ (pack $ show $ model ^. numberOfGames) []--[ellipsis]
      ] `styleBasic` [paddingV 10],

      hstack [
        inputFieldText "Server url",
        box (textField serverUrl) `styleBasic` [paddingH 10]
      ] `styleBasic` [paddingV 10],

      hstack [
        filler, 
        button "Start game" AppStart `nodeEnabled` (model ^. myName /= "" && 
                                                    model ^. numberOfGames > 0 &&
                                                    not (model ^. gameInRun)) `styleBasic` [paddingH 10],
        filler,
        button "Get round:" AppGetRound `styleBasic` [paddingH 10],
        spacer,
        box (numericField roundId ) `styleBasic` [paddingH 10],
        filler,
        button "Get list" AppGetListOfRounds `styleBasic` [paddingH 10],
        filler
      ] `styleBasic` [paddingV 10],

      spacer `styleBasic` [borderB 1 darkSalmon],
      spacer,

      displayRound (model ^. roundContent),

      spacer,
    
      sampleTextLabel
    ] `styleBasic` [padding 10]

  -- | ===================================  titleText  ==================================================================
  titleText text = label text
    `styleBasic` [textFont "Medium", textSize 20]
  inputFieldText text = label text
    `styleBasic` [textFont "Regular", textSize 16]

  -- | ===================================  sampleTextLabel  ==================================================================
  sampleTextLabel = label_ (model ^. sampleText) [ellipsis,multiline]
    `styleBasic` [
      bgColor dimGray,
      border 4 lightGray,
      radius 10,
      textFont (model ^. fontName),
      textSize (model ^. fontSize),
      textColor (model ^. fontColor),
      textCenter,
      flexHeight 100]    

  valueStyle = [paddingH 15
               ,bgColor dimGray
               ,border 1 lightGray
               ,radius 5
               ]

  -- | ===================================  displayRound  ==================================================================
  displayRound maybeRound = case maybeRound of
    Nothing -> vstack []
    Just r  -> vstack [
      hstack [
        inputFieldText "Player 1",
        titleText (pack $ show $ fromMaybe "" $ r ^. player1) `styleBasic` valueStyle,
        filler,
        inputFieldText "Player 2",
        titleText (pack $ show $ fromMaybe "" $ r ^. player2) `styleBasic` valueStyle
        ] `styleBasic` [paddingV 10],

      spacer,

      hstack [
        inputFieldText "Round type",
        titleText (pack $ show $ r ^. roundType) `styleBasic` valueStyle
        ] `styleBasic` [paddingV 10],

      spacer,

      hstack [
        inputFieldText "Game #",
        titleText (pack $ show $ r ^. numberOfCurrentGame) `styleBasic` valueStyle,
        filler,
        inputFieldText "Result",
        titleText (pack $ show $ r ^. result) `styleBasic` valueStyle
        ] `styleBasic` [paddingV 10],

      spacer,

      hstack [
        inputFieldText "Round ID",
        titleText (pack $ show $ r ^. roundID) `styleBasic` valueStyle
        ] `styleBasic` [paddingV 10],

      spacer
      ] `styleBasic` [borderB 1 darkSalmon]

-- | ===================================  handleEvent  ==================================================================
handleEventGUI
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEventGUI wenv node model evt = case evt of
    AppInit               -> [Model $ model & gameInRun .~ False]
    AppStart              -> [Model $ model & gameInRun .~ True
                             ,Task $ postGameStart model]
    AppOnLeaveNbr         -> []
    AppGetRound           -> [Task  $ getRound model]
    AppGetListOfRounds    -> [Task  $ getListOfRounds model]
    AppSetRound r         -> [Task  $ maybePlayGame model
                              ,Model $ model & roundContent ?~ r & sampleText .~ "OK"
                             ]
    AppSetListOfRounds ls -> [Model $ model & sampleText   .~ ls]
    AppSetError t         -> [Model $ model & sampleText   .~ t]
  where 
    getRound model = do 
      let url = "http://" ++ unpack (model ^. serverUrl) ++ ":8080/?round=" ++ show (model ^. roundId)
      --r <- asJSON =<< get uri -- :: IO (Either e (Response Universe))
      rb <- get url -- `E.catch` handler
      case asJSON rb of
        Right rr  -> return $ AppSetRound $ rr ^. responseBody
        Left err -> return $ AppSetError $ decodeUtf8 $ BL.toStrict $ rb ^. responseBody -- pack $ show err

    postGameStart model = do
      let url = "http://" ++ unpack (model ^. serverUrl) ++ ":8080/join"
      let opts = defaults & param "name" .~ [model ^. myName] & param "round" .~ [pack $ show $ model ^. numberOfGames]
      rb <- postWith opts url BL.empty
      --return $ AppSetError $ decodeUtf8 $ BL.toStrict $ rb ^. responseBody
      case asJSON rb of
        Right rr  -> return $ AppSetRound $ rr ^. responseBody
        Left err -> return $ AppSetError $ decodeUtf8 $ BL.toStrict $ rb ^. responseBody

    getListOfRounds model = do
      let url = "http://" ++ unpack (model ^. serverUrl) ++ ":8080/"
      rb <- get url -- `E.catch` handler
      case asJSON rb of
        Right rr  -> return $ AppSetListOfRounds $ rr ^. responseBody
        Left err -> return $ AppSetError $ decodeUtf8 $ BL.toStrict $ rb ^. responseBody -- pack $ show err

    maybePlayGame model = do
      case model ^. roundContent of
        Nothing -> return AppInit
        Just ro -> case ro ^. player2 of
          Nothing -> return AppInit
          Just s -> E.catch (playGame ro >> return AppInit) 
                          (\e -> do let err = show (e :: E.IOException)
                                    hPutStr stderr ("Error: " ++ err)
                                    return AppInit)



-- | ===================================  main  ==================================================================
main :: IO ()
main = do
  startApp model handleEventGUI buildUI config
  where
    config = [
      appWindowState $ MainWindowNormal (450,500),
      appWindowTitle "Foot",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appFontDef "Italic" "./assets/fonts/Roboto-Italic.ttf",
      appInitEvent AppInit
      ]
    model = AppModel {
      _serverUrl = "localhost",
      _myName = "",
      _numberOfGames = 0,
      _roundId = 0,
      _roundContent = Nothing,
      _gameInRun = False,
      _sampleText = "Hello World!",
      _showPicker = False,
      _fontName = "Regular",
      _fontSize = 20,
      _fontColor = white
    }


-- | ===================================   ==================================================================
theSize :: Int
theSize = 8

window :: Draw.World -> G.Display
window w = G.InWindow "Foot" (w ^. Draw.width, w ^. Draw.height) (w ^. Draw.offset, w ^. Draw.offset)
     
background :: G.Color
background = G.black
-- | ===================================  playGame  ==================================================================
playGame :: Round -> IO ()
playGame ro = G.play (window w) background 0 w Draw.drawWorld handleEvent iterateWorld
    where
        iterateWorld _ w = w
        -- Initialize World and update it by current game
        w = (Draw.initWorld theSize) {Draw._currentRound = ro}
