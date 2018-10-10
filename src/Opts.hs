{-# LANGUAGE OverloadedStrings #-}

module Opts ( Options
            , getOpts
            , optVerbose
            , webPort
            , dbLocation
            ) where
 
import Prelude
import System.Console.GetOpt
import System.Environment    (getProgName, getArgs)
import System.Exit           (exitSuccess)
import System.IO             (hPutStrLn, stderr)

import qualified System.Directory as Dir

data Options = Options { optVerbose :: Bool
                       , webPort    :: Int
                       , dbLocation :: IO FilePath
                       }

defaultOptions :: Options
defaultOptions = Options { optVerbose = False
                         , webPort    = 3000
                         , dbLocation = Dir.getXdgDirectory Dir.XdgData "hummingway"
                         }

version :: String
version = "0.1.0.0"

printVersion :: Options -> IO Options
printVersion _ = do
    hPutStrLn stderr $ "Version " ++ version
    exitSuccess

printHelp :: Options -> IO Options
printHelp _ = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    exitSuccess

databaseLocation :: FilePath -> Options -> IO Options
databaseLocation arg opt = return opt { dbLocation = Dir.makeAbsolute arg }

verbose :: Options -> IO Options
verbose opt = return opt { optVerbose = True }

setWebPort :: String -> Options -> IO Options
setWebPort arg opt = return opt { webPort = read arg :: Int }

blank :: OptDescr (Options -> IO Options)
blank = Option [] [] (NoArg return) ""

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option ['v'] ["verbose"]
        (NoArg verbose) "Enable verbose messages"

    , Option ['V'] ["version"]
        (NoArg printVersion) "Print version"

    , Option ['h', '?'] ["help"]
        (NoArg printHelp) "Show help"

    , blank

    , Option ['d'] ["db"]
        (ReqArg databaseLocation "DATABASE") "Database location"

    , Option ['p'] ["webport"]
        (ReqArg setWebPort "PORT") "Port for the webserver to run on"
    ]

getOpts :: IO Options
getOpts = do
    args <- getArgs
    let (actions, _nonoptions, _errors) = getOpt RequireOrder options args
    foldl (>>=) (return defaultOptions) actions
