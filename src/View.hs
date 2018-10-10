{-# LANGUAGE OverloadedStrings #-}

module View where

import Prelude

import Control.Monad ( forM_, mapM_ )

import Text.Blaze.Internal ( stringValue )
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import qualified Model.Persist as Model
import qualified Database.Persist.Sqlite as SQL ( fromSqlKey )

-- Helpers

jsCall :: Show a => String -> a -> AttributeValue
jsCall f arg = stringValue $ f ++ "(" ++ show arg ++ ")"

jsCall2 :: Show a => String -> a -> a -> AttributeValue
jsCall2 f arg1 arg2 = stringValue $ f ++ "(" ++ show arg1 ++ "," ++ show arg2 ++ ")"

-- Partials

headerNormal :: Html
headerNormal = H.head $ do
    script ! src "/races.js" $ ""
    link ! rel "stylesheet" ! href "/main.css"

-- Public views

races :: [Model.RaceInfo] -> Html
races ri = html $ do
    headerNormal
    H.body $ H.table $
        forM_ ri $ \(rid, r, pl) ->
            tr ! onclick (jsCall "openRace" $ SQL.fromSqlKey rid) $ mapM_ td
                [ H.span (toHtml $ show $ Model.raceCreatedAt r)
                , forM_ pl (\(_, p') -> H.span (toHtml $ Model.playerName p'))
                ]

race :: Maybe Model.RaceInfo -> Html
race Nothing = H.body $ H.span "Invalid race"
race (Just (_i, _r, pl)) = html $ do
    headerNormal
    H.body ! class_ "cell" $ H.table $ tr $
        forM_ pl (\(pid, p') -> td ! onclick (jsCall "viewTracker" $ SQL.fromSqlKey pid) $ H.span (toHtml $ Model.playerName p'))

racer :: Maybe Model.PlayerTracker -> Html
racer Nothing = H.body $ H.span "Invalid racer"
racer (Just (pid, tid, _pl)) = html $ do
    headerNormal
    H.body ! class_ "tracker" ! onload (jsCall2 "startTracker" (SQL.fromSqlKey pid) (SQL.fromSqlKey tid)) $ H.div ! A.id (stringValue "trackerCanvas") $ ""

-- Admin Partials

headerAdmin :: Html
headerAdmin = H.head $ do
    script ! src "/admin.js" $ ""
    link ! rel "stylesheet" ! href "/main.css"

trackerRowsAdmin :: Model.RowInfo -> Html
trackerRowsAdmin (rid, _, cis) = tr $ do
    mapM_ trackerCellsAdmin cis
    td ! onclick (jsCall "addCell" $ SQL.fromSqlKey rid) $ "Add cell"
    if null cis then td ! onclick (jsCall "deleteRow" $ SQL.fromSqlKey rid) $ "Delete row"
    else td ""

trackerCellsAdmin :: Model.CellInfo -> Html
trackerCellsAdmin (cid, c) = td ! onclick (jsCall "editCell" $ SQL.fromSqlKey cid) $ H.table $ do
    tr $ caption $ toHtml $ Model.cellName c
    tr $ td $ img ! src (stringValue $ Model.cellIcon c)

-- Admin Views

admin :: [Model.RaceInfo] -> [Model.TrackerInfo] -> Html
admin rs ts = html $ do
    headerAdmin
    H.body ! class_ "admin row" $ do
        H.table $ do
            tr $ caption ! colspan "2" $ H.span "Races"
            forM_ rs $ \(rid, r, pl) ->
                tr ! onclick (jsCall "openRace" $ SQL.fromSqlKey rid) $ mapM_ td
                    [ H.span (toHtml $ Model.raceGameKey r)
                    , forM_ pl (\(_, p') -> H.span (toHtml $ Model.playerName p'))
                    ]
        H.table $ do
            tr $ caption $ H.span "Trackers"
            forM_ ts $ \(tid, t, _) ->
                tr ! onclick (jsCall "openTracker" $ SQL.fromSqlKey tid) $ td $ H.span (toHtml $ Model.trackerGame t)

raceAdmin :: Maybe Model.RaceInfo -> Maybe Model.TrackerInfo -> Html
raceAdmin Nothing _ = html $ H.body $ H.span "Game not found"
raceAdmin _ Nothing = html $ H.body $ H.span "Tracker not found for game!  Database is likely corrupted."
raceAdmin (Just (rid, r, pl)) (Just (tid, t, ris)) = html $ do
    headerAdmin
    H.body ! class_ "admin" ! onload (jsCall "startTracker" (SQL.fromSqlKey rid)) $ H.table $
        tr $ forM_ pl $ \(pi, p') ->
            table ! A.id (stringValue $ "racer_" ++ show (SQL.fromSqlKey pi)) $ do
                tr $ caption $ toHtml $ Model.playerName p'
                tr $ td $ racePlayerAdmin ris pi

racePlayerAdmin :: [Model.RowInfo] -> Model.PlayerId -> Html
racePlayerAdmin ris pid = table $ forM_ ris $
    \(rid, _r, ci) -> tr $ forM_ ci $
        \(cid, c) -> td ! A.id (stringValue $ show (SQL.fromSqlKey cid)) ! A.class_ "inactive" ! onclick (jsCall2 "toggle" (SQL.fromSqlKey cid) (SQL.fromSqlKey pid)) $ img ! src (stringValue $ Model.cellIcon c)

trackerAdmin :: Maybe Model.TrackerInfo -> Html
trackerAdmin Nothing = html $ H.body $ H.span "Tracker not found"
trackerAdmin (Just (tid, t, ris)) = html $ do
    headerAdmin
    H.body ! class_ "admin cell" $ H.table $ do
        tr $ caption $ toHtml $ Model.trackerGame t
        mapM_ trackerRowsAdmin ris
        tr $ td ! colspan "100" ! onclick (jsCall "addRow" $ SQL.fromSqlKey tid) $ "Add row to tracker"

cellAdmin :: Maybe Model.CellInfo -> Integer -> Html
cellAdmin Nothing _ = html $ H.body $ H.span "Cell not found"
cellAdmin (Just (cid, c)) tid = html $ do
    headerAdmin
    H.body $ do
        H.form ! action (stringValue "/admin/cell") ! method "post" ! enctype "multipart/form-data" $ H.table $ do
            input ! type_ "hidden" ! A.id "id" ! name "id" ! value (stringValue $ show (SQL.fromSqlKey cid))
            input ! type_ "hidden" ! A.id "tracker" ! name "tracker" ! value (stringValue $ show tid)
            tr $ td $ input ! type_ "text" ! A.id "name" ! name "name" ! placeholder "Sprite Name" ! value (stringValue $ Model.cellName c)
            tr $ td $ input ! type_ "file" ! A.id "icon" ! name "icon" ! placeholder "Sprite File"
            tr $ td img ! src (stringValue $ Model.cellIcon c)
            tr $ td $ input ! type_ "submit"
        button ! onclick (jsCall "delCell" $ SQL.fromSqlKey cid) $ "Delete Cell"
