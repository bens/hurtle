module System.Hurtle.Log where

import           System.Hurtle.Common
import           Text.Printf

data Log e
    = Enqueued [CallId] Component
    | Finished Component
    | GotLock Component
    | NoHandlerFound CallId
    | ReleasedLock Component
    | Retrying CallId
    | RevertingState Component
    | Sending CallId Component
    | SystemError e Component
    | Waiting Component
      deriving (Eq, Show)

data Level
    = Debug | Info | Warning | Error
      deriving (Eq, Ord, Show)

-- | Hurtle is implemented with two worker threads, the 'Sender' and 'Receiver'.
-- Adding this bit of data to logs clears up which has got what lock when.
data Component
    = Main | Sender | Receiver
      deriving (Eq, Show)

logLevel :: Log e -> Level
logLevel msg = case msg of
    Enqueued _ _     -> Debug
    Finished _       -> Info
    GotLock _        -> Debug
    NoHandlerFound _ -> Error
    ReleasedLock _   -> Debug
    Retrying _       -> Warning
    RevertingState _ -> Warning
    Sending _ _      -> Debug
    SystemError _ _  -> Error
    Waiting _        -> Debug

logComponent :: Log e -> Component
logComponent msg = case msg of
    Enqueued _ comp     -> comp
    Finished comp       -> comp
    GotLock comp        -> comp
    NoHandlerFound _    -> Receiver
    ReleasedLock comp   -> comp
    Retrying _          -> Receiver
    RevertingState comp -> comp
    Sending _ comp      -> comp
    SystemError _ comp  -> comp
    Waiting comp        -> comp

logDescription :: (e -> String) -> Log e -> String
logDescription showE msg = case msg of
    Enqueued cids _    -> printf "enqueued %s" (show cids)
    Finished _         -> "finished"
    GotLock _          -> ">>>"
    NoHandlerFound cid -> printf "no handler found! (%s)" (show cid)
    ReleasedLock _     -> "<<<"
    Retrying cid       -> printf "retrying %s..." (show cid)
    RevertingState _   -> "reverting state"
    Sending cid _      -> printf "sending %s..." (show cid)
    SystemError e _    -> showE e
    Waiting _          -> "waiting..."
