{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Hurtle.Common where

import           Data.Hashable (Hashable)

data Box f where Box :: f a -> Box f
data Box2 f where Box2 :: f a b -> Box2 f

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
