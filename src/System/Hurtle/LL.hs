{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module System.Hurtle.LL (Forkable(..), LL, runLL, makeCall) where

import           Control.Applicative
import qualified Control.Concurrent         as Conc
import qualified Control.Concurrent.Async   as Async
import qualified Control.Concurrent.STM     as STM
import qualified Control.Exception          as Exc
import           Control.Lens               hiding (Level, (<|))
import           Control.Monad              (guard, when)
import           Control.Monad.Fix          (fix)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Morph        (hoist, lift)
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State  hiding (state)
import           Control.Monad.Trans.Writer
import           Data.Foldable              (mapM_)
import qualified Data.HashMap.Strict        as Hash
import           Data.Sequence              (ViewR (..), (<|))
import qualified Data.Sequence              as Seq

import           Prelude                    hiding (mapM_)

import           System.Hurtle.Common
import           System.Hurtle.Log

data Queued a = Queued CallId a

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
    , _llQueue    :: Seq.Seq (Queued (LL t t' e ()))
    , _llInFlight :: Hash.HashMap CallId (SomeRequest t t' e)
    }
makeLenses ''LLState

makeCall :: t -> (t' -> Either e (LL t t' e a)) -> LL t t' e a
makeCall x k = LL . FreeT . return . Free $ LLF x (either err unLL . k)
  where
    err = FreeT . return . Free . Throw

-- Spawn a new concurrent thread.
llFork :: (Functor m, Monad m)
       => LL t t' e () -> StateT (LLState st t t' e) m CallId
llFork x = do
    st <- get
    let st' = st & llNextId +~ 1
                 & llQueue  %~ (Queued (CallId (st ^. llNextId)) x <|)
    CallId (st ^. llNextId) <$ put st'

-- Pull the next fresh action from the queue.
llUnqueue :: (Functor m, Monad m)
          => StateT (LLState st t t' e) m (Maybe (CallId, LL t t' e ()))
llUnqueue = do
    st <- get
    case Seq.viewr (st ^. llQueue) of
        EmptyR -> return Nothing
        queue :> Queued cid x -> Just (cid, x) <$ put (st & llQueue .~ queue)

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
runLL Config{..} logIt ll  = do
    initialState <- configInit
    stateV <- STM.atomically $
        STM.newTMVar (LLState 0 initialState Seq.empty Hash.empty)
    begun <- STM.atomically newFlagPost
    done  <- STM.atomically newFlagPost

    let withSt = withStateV stateV logIt

    withSt Main $ do
        cid <- llFork ll
        lift . logIt $ Enqueued [cid] Main

    let doStep category cid (LL (FreeT m)) = do
            (resp, enqs) <- runWriterT (hoist lift m)
            cids <- mapM llFork enqs
            lift . logIt $ Enqueued cids category
            case resp of
                Pure () -> return ()
                Free (LLF t kont) -> do
                    st <- use llState
                    lift . logIt $ Sending cid category
                    lift (configSend st cid t)
                    llSent cid t (LL . kont)
                Free (Throw e) ->
                    lift . logIt $ SystemError e category

    -- Launch each enqueued chain.
    sender <- Async.async . fix $ \loop -> do
        let category = Sender
            onDone = Nothing <$ waitFlagPost done
            getNext = do
                (xM, state) <- runState llUnqueue <$> STM.takeTMVar stateV
                maybe STM.retry (\x -> Just x <$ STM.putTMVar stateV state) xM
        logIt $ Waiting category
        next <- STM.atomically $ onDone <|> getNext
        case next of
            Nothing -> logIt $ Finished category
            Just (cid, m) -> do
                withSt category $ doStep category cid m
                STM.atomically $ flagPost begun
                Conc.yield >> loop

    -- Receive a reply and run the next step of a chain.
    receiver <- Async.async . fix $ \loop -> do
        let category = Receiver
            onDone = Nothing <$ waitFlagPost done
            getState = do
                waitFlagPost begun
                state <- STM.readTMVar stateV
                let inFlight = state ^. llInFlight
                    queue    = state ^. llQueue
                if Hash.null inFlight && not (Seq.null queue)
                    then STM.retry
                    else return (Just (state ^. llState))
        stM <- STM.atomically $ onDone <|> getState
        case stM of
            Nothing -> logIt $ Finished category
            Just st -> do
                resp <- configRecv st
                continue <- withSt category $ case resp of
                    Ok cid t -> do
                        reqM <- llFindRequest cid
                        case reqM of
                            Nothing -> lift . logIt $ NoHandlerFound cid
                            Just (SR kont _) -> doStep category cid (kont t)
                        return True
                    Retry cid stM' -> do
                        lift . logIt $ Retrying cid
                        mapM_ (llState .=) stM'
                        reqM <- llFindRequest cid
                        case reqM of
                            Nothing -> lift . logIt $ NoHandlerFound cid
                            Just (SR kont t) -> llSent cid t kont
                        return True
                    LogError e ->
                        -- TODO: what else??
                        True <$ lift (logIt $ SystemError e category)
                    Fatal e ->
                        False <$ lift (logIt $ SystemError e category)
                when continue $
                    Conc.yield >> loop

    mapM_ Async.link [sender, receiver]
    let cleanup = do
            state <- STM.atomically (STM.readTMVar stateV)
            configTerm (state ^. llState)
    flip Exc.finally cleanup $ do
        logIt $ Waiting Main
        STM.atomically $ do
            waitFlagPost begun
            state <- STM.readTMVar stateV
            guard (Seq.null  (state ^. llQueue))
            guard (Hash.null (state ^. llInFlight))
            flagPost done
        mapM_ Async.wait [sender, receiver]
        logIt $ Finished Main


--
-- UTILITIES
--

newtype FlagPost = FlagPost (STM.TMVar ())

newFlagPost :: STM.STM FlagPost
newFlagPost = FlagPost <$> STM.newEmptyTMVar

flagPost :: FlagPost -> STM.STM ()
flagPost (FlagPost v) = () <$ STM.tryPutTMVar v ()

waitFlagPost :: FlagPost -> STM.STM ()
waitFlagPost (FlagPost v) = STM.readTMVar v

withStateV :: STM.TMVar (LLState st t t' e)
           -> (Log e -> IO ())           -- ^ Log handler
           -> Component
           -> StateT (LLState st t t' e) IO c -> IO c
withStateV stateV logIt cat m = do
    state <- STM.atomically $ STM.takeTMVar stateV
    let cleanup = do
            logIt $ RevertingState cat
            STM.atomically (STM.putTMVar stateV state)
    flip Exc.onException cleanup $ do
        logIt $ GotLock cat
        (x, state') <- runStateT m state
        logIt $ ReleasedLock cat
        x <$ STM.atomically (STM.putTMVar stateV state')

--
-- Forkable
--

class Monad m => Forkable m where
    fork :: m () -> m ()

instance Forkable (LL t t' e) where
    fork m = LL $ FreeT (Pure () <$ tell [m])

instance Forkable IO where
    fork m = () <$ Conc.forkIO m
