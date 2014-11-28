{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module System.Hurtle
  ( -- * RequestsT Transformer
    RequestsT(..), request
    -- * Hurtle
  , Hurtle, runHurtle, makeCall
    -- * Backends
  , LL.Forkable(..), LL.LogHandler
  , module System.Hurtle.Common
    -- * Logging
  , module System.Hurtle.Log
  ) where

import           Control.Applicative
import           Control.Monad             (ap)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))

import           System.Hurtle.Common
import           System.Hurtle.LL          (LL)
import qualified System.Hurtle.LL          as LL
import           System.Hurtle.Log

newtype RequestsT i m a
    = RequestsT{ unRequestsT :: (i -> m (LL.Final m)) -> m a }

instance Functor m => Functor (RequestsT i m) where
    fmap f (RequestsT k) = RequestsT (fmap f . k)

instance (Functor m, Monad m) => Applicative (RequestsT i m) where
    pure = return
    (<*>) = ap

instance (Functor m, Monad m) => Monad (RequestsT i m) where
    return = lift . return
    RequestsT k >>= f = RequestsT $ \m -> k m >>= \x -> unRequestsT (f x) m

instance MonadTrans (RequestsT i) where
    lift = RequestsT . const

instance (Functor m, MonadIO m) => MonadIO (RequestsT i m) where
    liftIO = lift . liftIO

request :: LL.Forkable m => i -> RequestsT i m ()
request i = RequestsT $ \f -> LL.fork (f i)

type Hurtle c i r = RequestsT i (LL c r)

makeCall :: (Connection c, Monad (M c)) => Request c a -> Hurtle c i r a
makeCall x = lift $ LL.makeCall x

runHurtle :: (Connection c, Functor (M c), Monad (M c))
          => InitArgs c                 -- ^ Configuration
          -> (a -> M c ())              -- ^ Final action for each input
          -> LL.LogHandler c            -- ^ Log handler
          -> [i]                        -- ^ Initial values to enqueue
          -> (i -> Hurtle c i a a)      -- ^ Action for each input
          -> M c ()
runHurtle _ _ _ [] _ = return ()
runHurtle cfg finish logIt (x:xs) f = do
    let f' = flip unRequestsT f' . f
        start = mapM_ (LL.fork . f') xs >> f' x
    LL.runLL cfg finish logIt $ unRequestsT (lift start) f'
