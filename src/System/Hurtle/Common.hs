{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Hurtle.Common where

import           Data.Hashable (Hashable)

newtype CallId
    = CallId Integer deriving (Eq, Show, Hashable)

data Config t t' e = forall st. Config
    { configInit :: IO st
    , configTerm :: st -> IO ()
    , configSend :: st -> CallId -> t -> IO ()
    , configRecv :: st -> IO (Response st t' e)
    }

data Response st t' e
    = Ok CallId t'
    | Retry CallId (Maybe st)
    | LogError e
    | Fatal e
