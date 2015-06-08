{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Map as Map
import Data.Map (Map)
import System.IO
import Control.Exception
import Network
import Control.Monad
import Text.Printf
import Text.Read (readMaybe)

data Command = CJob    String
             | CGet    Int
             | CReady  Int
             | CList
             | CQuit 

data Job = Job
  { idnr   :: Int                          -- Jobnummer
  , task   :: String                       -- String-repräsentation der Aufgabe (z.B. "7.2 + 4")
  , result :: MVar (Either String Double)  -- Left: Fehlschlag, Right: Resultat
  , done   :: MVar ()                      -- "Fortschrittsanzeige". Leer wenn noch nicht fertig. 
  }

data Client = Client
  { clientHandle :: Handle
  , clientJobs   :: MVar [Job]  -- Warum genügen MVars? Warum hier nicht wie in der Vorlesung STM?
  }

-- Ähnlich wie in der Vorlesung
main :: IO ()
main = withSocketsDo $ do
           sock <- listenOn (PortNumber (fromIntegral port))
           printf "Listening on port %d\n" port
           forever $ do
               (handle, host, port) <- accept sock
               printf "Accepted connection from %s: %s\n" host (show port)
               nojobs <- newMVar []
               let newClient = Client handle nojobs
               forkFinally (runClient newClient) (\_ -> hClose handle)

-- Beliebig
port :: Int
port = 44444

runClient :: Client -> IO ()
runClient client@Client{..} = do
    msg <- hGetLine clientHandle
    continue <- handleMessage client msg
    when continue $ runClient client

-- Ähnlich zur Vorlesung.
-- True zurück geben, wenn noch weitere Nachrichten gehandelt werden sollen.
handleMessage :: Client -> String -> IO Bool
handleMessage client@Client{..} msg = error "TODO"

-- Hilfsfunktion
parseCommand :: String -> Either String Command
parseCommand msg = case words msg of
      ["job", rest]  -> Right (CJob rest)
      ["get", nr]    -> case (readMaybe nr) :: Maybe Int of
                             (Just num) -> Right (CGet num)
                             Nothing    -> Left  $ "  [ERROR] not an Int: " ++ nr
      ["ready", nr]  -> case (readMaybe nr) :: Maybe Int of
                             (Just num) -> Right (CReady num)
                             Nothing    -> Left  $ "  [ERROR] not an Int: " ++ nr
      ["list"]       -> Right (CList)
      ["quit"]       -> Right (CQuit)
      _              -> Left  $ "  [ERROR] invalid command: " ++ msg

