{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}

module System.Hurtle.Log where

import           Text.Printf

data Log e i
    = Finished
    | GotLock
    | NoHandlerFound i
    | ReleasedLock
    | Retrying i
    | Sending i
    | SystemError e
    | Waiting
      deriving (Show, Functor)

data Level
    = Debug | Info | Warning | Error
      deriving (Eq, Ord, Show)

logLevel :: Log e i -> Level
logLevel msg = case msg of
    Finished         -> Info
    GotLock          -> Debug
    NoHandlerFound _ -> Error
    ReleasedLock     -> Debug
    Retrying _       -> Warning
    Sending _        -> Debug
    SystemError _    -> Error
    Waiting          -> Debug

logDescription :: Show i => (e -> String) -> Log e i -> String
logDescription showE msg = case msg of
    Finished           -> "finished"
    GotLock            -> ">>>"
    NoHandlerFound cid -> printf "no handler found! (%s)" (show cid)
    ReleasedLock       -> "<<<"
    Retrying cid       -> printf "retrying %s..." (show cid)
    Sending cid        -> printf "sending %s..." (show cid)
    SystemError e      -> showE e
    Waiting            -> "waiting..."
