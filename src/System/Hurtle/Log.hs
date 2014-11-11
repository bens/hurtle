module System.Hurtle.Log where

import           System.Hurtle.Common
import           Text.Printf

data Log e
    = Enqueued [CallId]
    | Finished
    | GotLock
    | NoHandlerFound CallId
    | ReleasedLock
    | Retrying CallId
    | RevertingState
    | Sending CallId
    | SystemError e
    | Waiting
      deriving (Eq, Show)

data Level
    = Debug | Info | Warning | Error
      deriving (Eq, Ord, Show)

logLevel :: Log e -> Level
logLevel msg = case msg of
    Enqueued _       -> Debug
    Finished         -> Info
    GotLock          -> Debug
    NoHandlerFound _ -> Error
    ReleasedLock     -> Debug
    Retrying _       -> Warning
    RevertingState   -> Warning
    Sending _        -> Debug
    SystemError _    -> Error
    Waiting          -> Debug

logDescription :: (e -> String) -> Log e -> String
logDescription showE msg = case msg of
    Enqueued cids      -> printf "enqueued %s" (show cids)
    Finished           -> "finished"
    GotLock            -> ">>>"
    NoHandlerFound cid -> printf "no handler found! (%s)" (show cid)
    ReleasedLock       -> "<<<"
    Retrying cid       -> printf "retrying %s..." (show cid)
    RevertingState     -> "reverting state"
    Sending cid        -> printf "sending %s..." (show cid)
    SystemError e      -> showE e
    Waiting            -> "waiting..."
