{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module System.Hurtle.LL (Forkable(..), LLT, runLL, makeCall) where

import           Control.Applicative
import qualified Control.Concurrent         as Conc
import           Control.Lens               hiding (Level, (<|))
import           Control.Monad              (liftM)
import           Control.Monad.Fix          (fix)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Morph        (hoist, lift)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State  hiding (state)
import           Control.Monad.Trans.Writer
import           Data.Foldable              (forM_, mapM_)
import qualified Data.HashMap.Strict        as Hash
import           Pipes                      ((>->))
import qualified Pipes                      as P

import           Prelude                    hiding (mapM_)

import           System.Hurtle.Common
import           System.Hurtle.Log

data SomeRequest t t' e m = SR (t' -> LLT t t' e m ()) t

data LLF t t' e a
    = LLF t (t' -> a)
    | Throw e

instance Functor (LLF t t' e) where
    fmap f (LLF p k) = LLF p (f . k)
    fmap _ (Throw e) = Throw e

newtype LLT t t' e m a =
    LLT{ unLL :: FreeT (LLF t t' e) (WriterT [LLT t t' e m ()] m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (LLT t t' e) where
    lift = LLT . FreeT . lift . liftM Pure

data LLState st t t' e m = LLState
    { _llNextId   :: Integer
    , _llState    :: st
    , _llInFlight :: Hash.HashMap CallId (SomeRequest t t' e m)
    }
makeLenses ''LLState

makeCall :: Monad m
         => t -> (t' -> Either e (LLT t t' e m a)) -> LLT t t' e m a
makeCall x k = LLT . FreeT . return . Free $ LLF x (either err unLL . k)
  where
    err = FreeT . return . Free . Throw

-- Mark a request as being sent.
llSent :: (Functor m, Monad m)
       => CallId -> t -> (t' -> LLT t t' e m ())
       -> StateT (LLState st t t' e m) m ()
llSent cid t f = do
    st <- get
    put $ st & llInFlight %~ Hash.insert cid (SR f t)

-- Find a request and remove it from the in-flight set.
llFindRequest :: (Functor m, Monad m)
              => CallId
              -> StateT (LLState st t t' e m) m (Maybe (SomeRequest t t' e m))
llFindRequest cid = do
    st <- get
    case st ^. llInFlight . at cid of
        Nothing -> return Nothing
        Just sr -> Just sr <$ put (st & llInFlight %~ Hash.delete cid)

runLL :: (Functor m, Monad m)
      => Config t t' e m   -- ^ Configuration
      -> (Log e -> m ())   -- ^ Log handler
      -> LLT t t' e m ()
      -> m ()
runLL Config{..} logIt ll = do
    initialSt <- configInit
    let initialState = LLState 0 initialSt Hash.empty

        starter = P.await >>= go >> starter
          where
            go (cidM, m) = do
                cid <- maybe (lift $ CallId <$> (llNextId <<+= 1)) return cidM
                go' (cid, m)
            go' (cid, LLT (FreeT m)) = do
                lift . lift . logIt $ GotLock
                (resp, forks) <- runWriterT (hoist (lift . lift) m)
                case resp of
                    Pure () -> forM_ forks $ \m' -> go (Nothing, m')
                    Free (Throw e) -> lift . lift . logIt $ SystemError e
                    Free (LLF t k) -> do
                        st  <- lift $ use llState
                        lift $ llInFlight %= Hash.insert cid (SR (LLT . k) t)
                        lift . lift $ configSend st cid t
                        forM_ forks $ \m' -> go (Nothing, m')
                lift . lift . logIt $ ReleasedLock


        receiver = fix $ \loop -> do
            st   <- lift $ use llState
            resp <- lift . lift $ configRecv st
            case resp of
                Ok cid t' -> do
                    reqM <- lift $ llFindRequest cid
                    case reqM of
                        Nothing -> lift . lift . logIt $ NoHandlerFound cid
                        Just (SR k _) -> P.yield (Just cid, k t') >> loop
                Retry cid stM -> do
                    lift . lift . logIt $ Retrying cid
                    lift $ mapM_ (llState .=) stM
                    reqM <- lift $ llFindRequest cid
                    case reqM of
                        Nothing -> lift . lift . logIt $ NoHandlerFound cid
                        Just (SR k t) -> lift $ llSent cid t k
                    loop
                LogError e -> do
                    lift . lift . logIt $ SystemError e
                    loop
                Fatal e ->
                    lift . lift . logIt $ SystemError e

        program = (P.yield (Nothing, ll) >> receiver) >-> starter

    finalState <- execStateT (P.runEffect program) initialState
    configTerm (finalState ^. llState)

--
-- Forkable
--

class Monad m => Forkable m where
    fork :: m () -> m ()

instance (Functor m, Monad m) => Forkable (LLT t t' e m) where
    fork m = LLT $ FreeT (Pure () <$ tell [m])

instance Forkable IO where
    fork m = () <$ Conc.forkIO m
