{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module System.Hurtle.LL (Forkable(..), LL, runLL, makeCall) where

import           Control.Applicative
import qualified Control.Concurrent         as Conc
import           Control.Lens               hiding (Level, (<|))
import           Control.Monad.Fix          (fix)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Morph        (hoist, lift)
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

data SomeRequest t t' e = SR (t' -> LL t t' e ()) t

data LLF t t' e a
    = LLF t (t' -> a)
    | Throw e

instance Functor (LLF t t' e) where
    fmap f (LLF p k) = LLF p (f . k)
    fmap _ (Throw e) = Throw e

newtype LL t t' e a =
    LL{ unLL :: FreeT (LLF t t' e) (WriterT [LL t t' e ()] IO) a }
    deriving (Functor, Applicative, Monad, MonadIO)

data LLState st t t' e = LLState
    { _llNextId   :: Integer
    , _llState    :: st
    , _llInFlight :: Hash.HashMap CallId (SomeRequest t t' e)
    }
makeLenses ''LLState

makeCall :: t -> (t' -> Either e (LL t t' e a)) -> LL t t' e a
makeCall x k = LL . FreeT . return . Free $ LLF x (either err unLL . k)
  where
    err = FreeT . return . Free . Throw

-- Mark a request as being sent.
llSent :: (Functor m, Monad m)
       => CallId -> t -> (t' -> LL t t' e ())
       -> StateT (LLState st t t' e) m ()
llSent cid t f = do
    st <- get
    put $ st & llInFlight %~ Hash.insert cid (SR f t)

-- Find a request and remove it from the in-flight set.
llFindRequest :: (Functor m, Monad m)
              => CallId
              -> StateT (LLState st t t' e) m (Maybe (SomeRequest t t' e))
llFindRequest cid = do
    st <- get
    case st ^. llInFlight . at cid of
        Nothing -> return Nothing
        Just sr -> Just sr <$ put (st & llInFlight %~ Hash.delete cid)

runLL :: Config t t' e     -- ^ Configuration
      -> (Log e -> IO ())  -- ^ Log handler
      -> LL t t' e ()
      -> IO ()
runLL Config{..} logIt ll = do
    initialSt <- configInit
    let initialState = LLState 0 initialSt Hash.empty

        starter = P.await >>= go >> starter
          where
            go (cidM, m) = do
                cid <- maybe (lift $ CallId <$> (llNextId <<+= 1)) return cidM
                go' (cid, m)
            go' (cid, LL (FreeT m)) = do
                liftIO . logIt $ GotLock
                (resp, forks) <- runWriterT (hoist (lift . lift) m)
                case resp of
                    Pure () -> forM_ forks $ \m' -> go (Nothing, m')
                    Free (Throw e) -> liftIO . logIt $ SystemError e
                    Free (LLF t k) -> do
                        st  <- lift $ use llState
                        lift   $ llInFlight %= Hash.insert cid (SR (LL . k) t)
                        liftIO $ configSend st cid t
                        forM_ forks $ \m' -> go (Nothing, m')
                liftIO . logIt $ ReleasedLock

        receiver = fix $ \loop -> do
            st   <- lift $ use llState
            resp <- liftIO $ configRecv st
            case resp of
                Ok cid t' -> do
                    reqM <- lift $ llFindRequest cid
                    case reqM of
                        Nothing -> liftIO . logIt $ NoHandlerFound cid
                        Just (SR k _) -> P.yield (Just cid, k t') >> loop
                Retry cid stM -> do
                    liftIO . logIt $ Retrying cid
                    lift $ mapM_ (llState .=) stM
                    reqM <- lift $ llFindRequest cid
                    case reqM of
                        Nothing -> liftIO . logIt $ NoHandlerFound cid
                        Just (SR k t) -> lift $ llSent cid t k
                    loop
                LogError e -> do
                    liftIO . logIt $ SystemError e
                    loop
                Fatal e ->
                    liftIO . logIt $ SystemError e

        program = (P.yield (Nothing, ll) >> receiver) >-> starter

    finalState <- execStateT (P.runEffect program) initialState
    liftIO $ configTerm (finalState ^. llState)

--
-- Forkable
--

class Monad m => Forkable m where
    fork :: m () -> m ()

instance Forkable (LL t t' e) where
    fork m = LL $ FreeT (Pure () <$ tell [m])

instance Forkable IO where
    fork m = () <$ Conc.forkIO m
