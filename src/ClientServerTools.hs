{-|
Module      : ClientServerTools
Description : ClientServerTools' main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module ClientServerTools
    where

import           Data.Text (Text,pack, unpack)
import          Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL (toStrict,empty) 
import qualified Data.ByteString.Char8 as CS (pack) 
import           Data.Aeson


encode2Text :: ToJSON a => a -> Text
encode2Text = decodeUtf8 . BL.toStrict . encode

-- | Size of the game.
theSize :: Int
theSize = 8
