{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Model.Persist where

import Prelude

import Control.Monad                ( mapM, forM )
import Control.Monad.IO.Class       ( liftIO )
import Control.Lens.Setter          ( set )
import Control.Monad.Logger         ( NoLoggingT )
import Control.Monad.Reader         ( ReaderT, ask )
import Control.Monad.Trans.Resource ( ResourceT )

import Data.Text                    ( Text, pack )
import Data.Time                    ( UTCTime, getCurrentTime )
import GHC.Generics                 ( Generic )

import Data.Aeson
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Race
    tracker TrackerId
    playerCount Int
    gameKey Text
    createdAt UTCTime
    deriving Show Generic
Player
    race RaceId
    name String
    state String
    deriving Show Generic
Tracker
    game String
    deriving Show Generic
Row
    tracker TrackerId
    yPos Int
    deriving Show Generic
Cell
    row RowId
    xPos Int
    name String
    icon String
    deriving Show Generic
|]

instance FromJSON Race
instance ToJSON Race

instance FromJSON Player
instance ToJSON Player

instance FromJSON Tracker
instance ToJSON Tracker

instance FromJSON Row
instance ToJSON Row

instance FromJSON Cell
instance ToJSON Cell

type Backend a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a
newtype Config = Config { dbPath :: String }

type RaceInfo = (RaceId, Race, [PlayerInfo])
type PlayerInfo = (PlayerId, Player)
type PlayerTracker = (PlayerId, TrackerId, Player)
type TrackerInfo = (TrackerId, Tracker, [RowInfo])
type RowInfo = (RowId, Row, [CellInfo])
type CellInfo = (CellId, Cell)

dbName :: String
dbName = "main.sqlite"

connect :: FilePath -> SqliteConnectionInfo
connect path = set fkEnabled True $ mkSqliteConnectionInfo $ pack $ path ++ "/" ++ dbName

init :: FilePath -> IO ()
init path = runSqliteInfo (connect path) $ runMigration migrateAll

transactionally :: String -> Backend a -> IO a
transactionally path = runSqliteInfo (connect path)

races :: ReaderT Config IO [RaceInfo]
races = do
    Config{dbPath} <- ask
    entities <- liftIO $ transactionally dbPath $ selectList [] []
    mapM players entities

race :: Integral i => i -> ReaderT Config IO (Maybe RaceInfo)
race i = do
    Config{dbPath} <- ask
    let i' = toSqlKey (fromIntegral i) :: RaceId
    entity <- liftIO $ transactionally dbPath $ get i'
    players' entity i'

players :: Entity Race -> ReaderT Config IO RaceInfo
players r = do
    Config{dbPath} <- ask
    let r' = entityVal r
        p = entityKey r
    ps <- liftIO $ transactionally dbPath $ selectList [PlayerRace ==. p] []
    let p' = (\entity -> (entityKey entity, entityVal entity)) <$> ps
    return (p, r', p')

players' :: Maybe Race -> RaceId -> ReaderT Config IO (Maybe RaceInfo)
players' Nothing _ = return Nothing
players' (Just r) i = do
    Config{dbPath} <- ask
    ps <- liftIO $ transactionally dbPath $ selectList [PlayerRace ==. i] []
    let p = (\entity -> (entityKey entity, entityVal entity)) <$> ps
    return $ Just (i, r, p)

player :: Integral i => i -> ReaderT Config IO (Maybe PlayerTracker)
player i = do
    Config{dbPath} <- ask
    let i' = toSqlKey (fromIntegral i) :: PlayerId
    pe <- liftIO $ transactionally dbPath $ get i'
    tid <- playerTrackerId pe
    return $ tid >>= \t' ->
        pe >>= Just . (,,) i' t'

playerTrackerId :: Maybe Player -> ReaderT Config IO (Maybe TrackerId)
playerTrackerId Nothing = return Nothing
playerTrackerId (Just pl) = do
    Config{dbPath} <- ask
    r <- liftIO $ transactionally dbPath $ get $ playerRace pl
    let t = raceTracker <$> r
    return t

trackers :: ReaderT Config IO [TrackerInfo]
trackers = do
    Config{dbPath} <- ask
    ts <- liftIO $ transactionally dbPath $ selectList [] []
    forM ts $ \t -> do
        let t' = entityVal t
            k = entityKey t
        rows' t' k

tracker :: Integral i => i -> ReaderT Config IO (Maybe TrackerInfo)
tracker i = do
    Config{dbPath} <- ask
    let i' = toSqlKey (fromIntegral i) :: TrackerId
    entity <- liftIO $ transactionally dbPath $ get i'
    rows entity i'

trackerFor :: Maybe RaceInfo -> ReaderT Config IO (Maybe TrackerInfo)
trackerFor Nothing = return Nothing
trackerFor (Just (_, r, _)) = do
    Config{dbPath} <- ask
    let i' = raceTracker r
    entity <- liftIO $ transactionally dbPath $ get i'
    rows entity i'

rows :: Maybe Tracker -> TrackerId -> ReaderT Config IO (Maybe TrackerInfo)
rows Nothing _ = return Nothing
rows (Just t) i = do
    r <- rows' t i
    return $ Just r

rows' :: Tracker -> TrackerId -> ReaderT Config IO TrackerInfo
rows' t i = do
    Config{dbPath} <- ask
    rs <- liftIO $ transactionally dbPath $ selectList [RowTracker ==. i] []
    r <- mapM cells rs
    return (i, t, r)

cells :: Entity Row -> ReaderT Config IO RowInfo
cells r = do
    Config{dbPath} <- ask
    let r' = entityVal r
        k = entityKey r
    cs <- liftIO $ transactionally dbPath $ selectList [CellRow ==. k] []
    let c = cellInfo <$> cs
    return (k, r', c)

cell :: Integral i => i -> ReaderT Config IO (Maybe CellInfo)
cell i = do
    Config{dbPath} <- ask
    let i' = toSqlKey (fromIntegral i) :: CellId
    entity <- liftIO $ transactionally dbPath $ get i'
    return $ cell' entity i'

cell' :: Maybe Cell -> CellId -> Maybe CellInfo
cell' Nothing _ = Nothing
cell' (Just c) i = Just (i, c)

cellInfo :: Entity Cell -> CellInfo
cellInfo c = let c' = entityVal c
                 i = entityKey c
             in (i, c')

-- Creation

newRace :: Key Tracker -> Int -> Text -> ReaderT Config IO ()
newRace trackerId playerCount key = do
    Config{dbPath} <- ask
    time <- liftIO getCurrentTime
    let r = Race trackerId playerCount key time
    liftIO $ transactionally dbPath $ insert_ r

newTracker :: String -> ReaderT Config IO ()
newTracker name = do
    Config{dbPath} <- ask
    let t = Tracker name
    liftIO $ transactionally dbPath $ insert_ t

newRow :: Integral i => i -> ReaderT Config IO ()
newRow i = do
    Config{dbPath} <- ask
    let i' = toSqlKey (fromIntegral i) :: TrackerId
    n <- liftIO $ transactionally dbPath $ count [TrackerId ==. i']
    liftIO $ transactionally dbPath $ insert_ $ Row i' n

delRow :: Integral i => i -> ReaderT Config IO ()
delRow i = do
    Config{dbPath} <- ask
    let i' = toSqlKey (fromIntegral i) :: RowId
    liftIO $ transactionally dbPath $ delete i'
    return ()

newCell :: Integral i => i -> ReaderT Config IO ()
newCell i = do
    Config{dbPath} <- ask
    let i' = toSqlKey (fromIntegral i) :: RowId
    n <- liftIO $ transactionally dbPath $ count [RowId ==. i']
    liftIO $ transactionally dbPath $ insert_ $ Cell i' n "" ""

delCell :: Integral i => i -> ReaderT Config IO ()
delCell i = do
    Config{dbPath} <- ask
    let i' = toSqlKey (fromIntegral i) :: CellId
    liftIO $ transactionally dbPath $ delete i'
    return ()

updateCell :: Integral i => i -> String -> String -> ReaderT Config IO ()
updateCell i name icon = do
    Config{dbPath} <- ask
    let i' = toSqlKey (fromIntegral i) :: CellId
    liftIO $ transactionally dbPath $ update i' [CellName =. name, CellIcon =. icon]
    return ()

setPlayerState :: Integral i => i -> String -> ReaderT Config IO ()
setPlayerState i blob = do
    Config{dbPath} <- ask
    let i' = toSqlKey (fromIntegral i) :: PlayerId
    liftIO $ transactionally dbPath $ update i' [PlayerState =. blob]
    return ()
