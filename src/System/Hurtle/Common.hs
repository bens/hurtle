{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Hurtle.Common where

import           Data.Hashable (Hashable)

newtype CallId
    = CallId Int deriving (Eq, Show, Hashable)

data Config t t' e m = forall st. Config
    { configInit :: m st
    , configTerm :: st -> m ()
    , configSend :: st -> CallId -> t -> m ()
    , configRecv :: st -> m (Response st t' e)
    }

data Response st t' e
    = Ok CallId t'
    | Retry CallId (Maybe st)
    | LogError e
    | Fatal e
