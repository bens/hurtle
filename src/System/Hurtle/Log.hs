{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}

module System.Hurtle.Log where

import           Text.Printf

data Log e i
    = Blocked i i
    | Continuing i i
    | Finished i
    | Forked i i
    | PropagatingError i i
    | Resumed i
    | Retrying i
    | Sending i
    | Starting i
    | SystemError e
    | SystemError' i e
      deriving (Show, Functor)

data Level
    = Debug | Info | Warning | Error
      deriving (Eq, Ord, Show)

logLevel :: Log e i -> Level
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

logDescription :: Show i => (e -> String) -> Log e i -> String
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
