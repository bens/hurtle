{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}

module System.Hurtle.Log
  ( Id, firstId, nextId
  , Log(..), Level(..)
  , logLevel, logDescription
  ) where

import           Text.Printf

newtype Id = Id Int deriving (Eq, Ord)

instance Show Id where show (Id x) = show x

firstId :: Id
firstId = Id 0

nextId :: Id -> Id
nextId (Id x) = Id (1+x)

data Log e
    = Blocked Id Id
    | Continuing Id Id
    | Finished Id
    | Forked Id Id
    | PropagatingError Id Id
    | Resumed Id
    | Retrying Id
    | Sending Id
    | Starting Id
    | SystemError e
    | SystemError' Id e
      deriving (Show, Functor)

data Level
    = Debug | Info | Warning | Error
      deriving (Eq, Ord, Show)

logLevel :: Log e -> Level
logLevel msg = case msg of
    Blocked _ _          -> Debug
    Continuing _ _       -> Debug
    Finished _           -> Debug
    Forked _ _           -> Debug
    PropagatingError _ _ -> Debug
    Resumed _            -> Debug
    Retrying _           -> Warning
    Sending _            -> Debug
    Starting _           -> Debug
    SystemError _        -> Error
    SystemError' _ _     -> Error

logDescription :: (e -> String) -> Log e -> String
logDescription showE msg = case msg of
    Blocked blocked on       -> printf "%s waiting for %s"
                                    (show blocked) (show on)
    Continuing cid on        -> printf "%s continuing with result from %s"
                                    (show cid) (show on)
    Finished cid             -> printf "%s finished" (show cid)
    Forked old new           -> printf "%s forked %s" (show old) (show new)
    PropagatingError from to -> printf "copying error from %s to %s"
                                    (show from) (show to)
    Resumed cid              -> printf "%s <--" (show cid)
    Retrying cid             -> printf "retrying %s..." (show cid)
    Sending cid              -> printf "%s -->" (show cid)
    Starting cid             -> printf "%s started" (show cid)
    SystemError e            -> showE e
    SystemError' cid e       -> printf "%s failed: %s" (show cid) (showE e)
