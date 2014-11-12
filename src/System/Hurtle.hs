module System.Hurtle
  ( RequestsT(..), request
  , Hurtle, runHurtle, makeCall
    -- * Backends
  , module System.Hurtle.Common
    -- * Logging
  , module System.Hurtle.Log
  ) where

import           Control.Applicative
import           Control.Monad             (ap)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))

import           System.Hurtle.Common
import           System.Hurtle.LL          (LLT)
import qualified System.Hurtle.LL          as LL
import           System.Hurtle.Log

newtype RequestsT i m a = RequestsT{ unRequestsT :: (i -> m ()) -> m a }

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

type Hurtle t t' e i m = RequestsT i (LLT t t' e m)

makeCall :: Monad m => t -> (t' -> Either e a) -> Hurtle t t' e i m a
makeCall x k = lift $ LL.makeCall x (fmap return . k)

runHurtle :: (Functor m, Monad m)
          => Config t t' e m            -- ^ Configuration
          -> (a -> m ())                -- ^ Final action for each input
          -> (Log e -> m ())            -- ^ Log handler
          -> [i]                        -- ^ Initial values to enqueue
          -> (i -> Hurtle t t' e i m a) -- ^ Action for each input
          -> m ()
runHurtle cfg finish logIt xs f = do
    let f' x = unRequestsT (f x >>= (lift . lift . finish)) f'
        start = lift $ mapM_ (LL.fork . f') xs
    LL.runLL cfg logIt $ unRequestsT start f'
