{-# LANGUAGE TemplateHaskell #-}

module Const where

import Prelude ( ($) )
import Data.Text.Lazy ( fromStrict )
import Data.Text.Internal.Lazy ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.FileEmbed  ( embedFile )

mainCSS :: Text
mainCSS = fromStrict $ decodeUtf8 $(embedFile "static/main.css")

racesJS :: Text
racesJS = fromStrict $ decodeUtf8 $(embedFile "static/races.js")

adminJS :: Text
adminJS = fromStrict $ decodeUtf8 $(embedFile "static/admin.js")

trackerJS :: Text
trackerJS = fromStrict $ decodeUtf8 $(embedFile "static/tracker.js")
