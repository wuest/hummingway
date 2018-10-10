{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude
import Data.Text.Lazy
import Control.Monad ( unless )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Reader ( ReaderT, runReaderT )
import qualified Data.ByteString.Lazy as B ( writeFile )
import qualified Data.ByteString.Char8 as BS ( unpack )
import qualified System.Directory as Dir
import qualified System.FilePath as SF ( takeExtension, (</>) )

import Web.Scotty.Trans
import Text.Blaze.Html5 ( Html )
import Text.Blaze.Html.Renderer.Text
import Network.Wai.Middleware.Static ( addBase, hasPrefix, noDots, staticPolicy, (>->) )
import Network.Wai.Parse ( FileInfo(..) )

import qualified Model.Persist as Model
import qualified Const
import qualified View
import qualified Opts

setupDataDir :: FilePath -> IO ()
setupDataDir path = do
    exists <- Dir.doesDirectoryExist path
    unless exists $ do
        putStrLn $ "Data directory (" ++ path ++ ") doesn't exist - creating..."
        Dir.createDirectory path

    exists' <- Dir.doesDirectoryExist (path ++ "/static")
    unless exists' $ do
        putStrLn $ "Data directory (" ++ path ++ "/static) doesn't exist - creating..."
        Dir.createDirectory (path ++ "/static")

dbConfig :: r -> ReaderT r m a -> m a
dbConfig = flip runReaderT

start :: Int -> FilePath -> IO ()
start port base =
  scottyT port (dbConfig $ Model.Config base) $ do
      middleware $ staticPolicy $ noDots >-> hasPrefix "static" >-> addBase base
      routes $ base SF.</> "static"

blaze :: Html -> ActionT Text (ReaderT Model.Config IO) ()
blaze = html . renderHtml

routes :: FilePath -> ScottyT Text (ReaderT Model.Config IO) ()
routes dataDir = do
-- Health Check

    get "/_ping" $ text "OK"

-- Static Content

    get "/main.css" $ do
        setHeader "Content-Type" "text/css"
        text Const.mainCSS

    get "/races.js" $ do
        setHeader "Content-Type" "application/javascript"
        text Const.racesJS

    get "/admin.js" $ do
        setHeader "Content-Type" "application/javascript"
        text Const.adminJS

    get "/tracker.js" $ do
        setHeader "Content-Type" "application/javascript"
        text Const.trackerJS

-- Public routes

    get "/" $ do
        races <- lift Model.races
        blaze $ View.races races

    get "/race/:id" $ do
        raceId <- param "id"
        race <- lift $ Model.race (read raceId :: Integer)
        blaze $ View.race race

    get "/racer/:id" $ do
        personId <- param "id"
        person <- lift $ Model.player (read personId :: Integer)
        blaze $ View.racer person

    get "/tracker/:id" $ do
        trackerId <- param "id"
        t <- lift $ Model.tracker (read trackerId :: Integer)
        blaze $ View.trackerAdmin t

-- API JSON routes

    get "/api/0.1.0/race/:id" $ do
        raceId <- param "id"
        race <- lift $ Model.race (read raceId :: Integer)
        json race

    get "/api/0.1.0/tracker/:id" $ do
        trackerId <- param "id"
        tracker <- lift $ Model.tracker (read trackerId :: Integer)
        json tracker

    get "/api/0.1.0/racer/:id" $ do
        personId <- param "id"
        person <- lift $ Model.player (read personId :: Integer)
        json person

-- Admin routes

    get "/admin" $ do
        races <- lift Model.races
        trackers <- lift Model.trackers
        blaze $ View.admin races trackers

    get "/admin/race/:id" $ do
        raceId <- param "id"
        race <- lift $ Model.race (read raceId :: Integer)
        tracker <- lift $ Model.trackerFor race
        blaze $ View.raceAdmin race tracker

    get "/admin/tracker/:id" $ do
        trackerId <- param "id"
        tracker <- lift $ Model.tracker (read trackerId :: Integer)
        blaze $ View.trackerAdmin tracker

    get "/admin/tracker/:id/addrow" $ do
        trackerId <- param "id"
        _ <- lift $ Model.newRow (read trackerId :: Integer)
        redirect $ pack $ "/admin/tracker/" ++ trackerId

    get "/admin/row/:id/del/:ret" $ do
        rowId <- param "id"
        trackerId <- param "ret"
        _ <- lift $ Model.delRow (read rowId :: Integer)
        redirect $ pack $ "/admin/tracker/" ++ trackerId

    get "/admin/row/:id/addcell/:ret" $ do
        rowId <- param "id"
        trackerId <- param "ret"
        _ <- lift $ Model.newCell (read rowId :: Integer)
        redirect $ pack $ "/admin/tracker/" ++ trackerId

    get "/admin/cell/:id/del/:ret" $ do
        cellId <- param "id"
        trackerId <- param "ret"
        _ <- lift $ Model.delCell (read cellId :: Integer)
        redirect $ pack $ "/admin/tracker/" ++ trackerId

    get "/admin/cell/:id/:ret" $ do
        cellId <- param "id"
        trackerId <- param "ret"
        cell <- lift $ Model.cell (read cellId :: Integer)
        blaze $ View.cellAdmin cell trackerId

    post "/admin/cell" $ do
        cellId <- param "id"
        trackerId <- param "tracker"
        name <- param "name"
        fs <- files
        newPath <- liftIO $ writeUpload cellId dataDir (Prelude.head fs)
        _ <- lift $ Model.updateCell (read cellId :: Integer) name newPath
        redirect $ pack $ "/admin/tracker/" ++ trackerId

    post "/admin/state" $ do
        playerId <- param "player"
        blob <- param "blob"
        _ <- lift $ Model.setPlayerState (read playerId :: Integer) blob
        blaze ""

writeUpload :: String -> String -> File -> IO String
writeUpload c d (_, fi) = do
    let content = fileContent fi
        ext = SF.takeExtension $ BS.unpack $ fileName fi
        name = d SF.</> c ++ ext
    _ <- B.writeFile name content
    return $ "/static" SF.</> c ++ ext

main :: IO ()
main = do
    opts <- Opts.getOpts
    let port = Opts.webPort opts
    db <- Opts.dbLocation opts
    _ <- setupDataDir db
    _ <- Model.init db
    putStrLn $ "Starting service on port " ++ show port ++ " with database at " ++ db
    start port db
