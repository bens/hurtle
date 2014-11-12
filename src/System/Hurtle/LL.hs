{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module System.Hurtle.LL (Forkable(..), LLT, runLL, makeCall) where

import           Control.Applicative
import qualified Control.Concurrent         as Conc
import           Control.Monad              (liftM)
import           Control.Monad.Fix          (fix)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Morph        (hoist, lift)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State  hiding (state)
import           Control.Monad.Trans.Writer
import           Data.Foldable              (forM_, mapM_)
import qualified Data.IntMap                as IM
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
    { _llNextId   :: Int
    , _llState    :: st
    , _llInFlight :: IM.IntMap (SomeRequest t t' e m)
    }

makeCall :: Monad m
         => t -> (t' -> Either e (LLT t t' e m a)) -> LLT t t' e m a
makeCall x k = LLT . FreeT . return . Free $ LLF x (either err unLL . k)
  where
    err = FreeT . return . Free . Throw

-- Find a request and remove it from the in-flight set.
llFindRequest :: (Functor m, Monad m)
              => CallId
              -> StateT (LLState st t t' e m) m (Maybe (SomeRequest t t' e m))
llFindRequest (CallId cid) = do
    st <- get
    case IM.lookup cid (_llInFlight st) of
        Nothing -> return Nothing
        Just sr -> Just sr <$ put st{
            _llInFlight = IM.delete cid (_llInFlight st)
            }

runLL :: (Functor m, Monad m)
      => Config t t' e m   -- ^ Configuration
      -> (Log e -> m ())   -- ^ Log handler
      -> LLT t t' e m ()
      -> m ()
runLL Config{..} logIt ll = do
    initialSt <- configInit
    let initialState = LLState 0 initialSt IM.empty

        starter = P.await >>= go >> starter
          where
            nextId = do
                st <- get
                _llNextId st <$ put st{ _llNextId = _llNextId st + 1 }
            go (cidM, m) = do
                cid <- maybe (lift nextId) return cidM
                go' (cid, m)
            go' (cid, LLT (FreeT m)) = do
                lift . lift . logIt $ GotLock
                (resp, forks) <- runWriterT (hoist (lift . lift) m)
                case resp of
                    Pure () -> forM_ forks $ \m' -> go (Nothing, m')
                    Free (Throw e) -> lift . lift . logIt $ SystemError e
                    Free (LLF t k) -> do
                        lift . modify $ \s -> s{
                            _llInFlight =
                                IM.insert cid (SR (LLT . k) t) (_llInFlight s)
                            }
                        lift . lift . logIt $ Sending (CallId cid)
                        st <- lift $ gets _llState
                        lift . lift $ configSend st (CallId cid) t
                        forM_ forks $ \m' -> go (Nothing, m')
                lift . lift . logIt $ ReleasedLock

        receiver = fix $ \loop -> do
            lift . lift . logIt $ Waiting
            resp <- lift (gets _llState >>= lift . configRecv)
            case resp of
                Ok (CallId cid) t' -> do
                    reqM <- lift $ llFindRequest (CallId cid)
                    case reqM of
                        Nothing ->
                            lift . lift . logIt $ NoHandlerFound (CallId cid)
                        Just (SR k _) ->
                            P.yield (Just cid, k t') >> loop
                Retry (CallId cid) stM -> do
                    lift . lift . logIt $ Retrying (CallId cid)
                    lift $ mapM_ (\x -> modify $ \s -> s{ _llState = x }) stM
                    reqM <- lift $ llFindRequest (CallId cid)
                    case reqM of
                        Nothing ->
                            lift . lift . logIt $ NoHandlerFound (CallId cid)
                        Just sr -> lift . modify $ \st -> st{
                            _llInFlight = IM.insert cid sr (_llInFlight st)
                            }
                    loop
                LogError e -> do
                    lift . lift . logIt $ SystemError e
                    loop
                Fatal e ->
                    lift . lift . logIt $ SystemError e

        program = (P.yield (Nothing, ll) >> receiver) >-> starter

    finalState <- execStateT (P.runEffect program) initialState
    configTerm (_llState finalState)
    logIt Finished

--
-- Forkable
--

class Monad m => Forkable m where
    fork :: m () -> m ()

instance (Functor m, Monad m) => Forkable (LLT t t' e m) where
    fork m = LLT $ FreeT (Pure () <$ tell [m])

instance Forkable IO where
    fork m = () <$ Conc.forkIO m
