{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module System.Hurtle
  ( Hurtle, runHurtle, makeCall, request
    -- * Backends
  , CallId, Config(..), Response(..)
    -- * Logging
  , Log(..), Level(..), Component(..)
  , logDescription, logLevel, logComponent
  ) where

import           Control.Applicative
import qualified Control.Concurrent         as Conc
import qualified Control.Concurrent.Async   as Async
import qualified Control.Concurrent.STM     as STM
import qualified Control.Exception          as Exc
import           Control.Lens               hiding (Level, (<|))
import           Control.Monad              (guard, when)
import           Control.Monad.Fix          (fix)
import           Control.Monad.Morph        (hoist, lift)
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State  hiding (state)
import           Control.Monad.Trans.Writer
import           Data.Foldable              (forM_, mapM_)
import           Data.Hashable              (Hashable)
import qualified Data.HashMap.Strict        as Hash
import           Data.Sequence              (ViewR (..), (<|))
import qualified Data.Sequence              as Seq
import           Text.Printf

import           Prelude                    hiding (mapM_)

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

data Level
    = Debug | Info | Warning | Error
      deriving (Eq, Ord, Show)

data Component
    = Main | Sender | Receiver
      deriving (Eq, Show)

data Log e
    = Enqueued [CallId] Component
    | Finished Component
    | GotLock Component
    | NoHandlerFound CallId
    | ReleasedLock Component
    | Retrying CallId
    | RevertingState Component
    | Sending CallId Component
    | SystemError e Component
    | Waiting Component
      deriving (Eq, Show)

logLevel :: Log e -> Level
logLevel msg = case msg of
    Enqueued _ _     -> Debug
    Finished _       -> Info
    GotLock _        -> Debug
    NoHandlerFound _ -> Error
    ReleasedLock _   -> Debug
    Retrying _       -> Warning
    RevertingState _ -> Warning
    Sending _ _      -> Debug
    SystemError _ _  -> Error
    Waiting _        -> Debug

logComponent :: Log e -> Component
logComponent msg = case msg of
    Enqueued _ comp     -> comp
    Finished comp       -> comp
    GotLock comp        -> comp
    NoHandlerFound _    -> Receiver
    ReleasedLock comp   -> comp
    Retrying _          -> Receiver
    RevertingState comp -> comp
    Sending _ comp      -> comp
    SystemError _ comp  -> comp
    Waiting comp        -> comp

logDescription :: (e -> String) -> Log e -> String
logDescription showE msg = case msg of
    Enqueued cids _    -> printf "enqueued %s" (show cids)
    Finished _         -> "finished"
    GotLock _          -> ">>>"
    NoHandlerFound cid -> printf "no handler found! (%s)" (show cid)
    ReleasedLock _     -> "<<<"
    Retrying cid       -> printf "retrying %s..." (show cid)
    RevertingState _   -> "reverting state"
    Sending cid _      -> printf "sending %s..." (show cid)
    SystemError e _    -> showE e
    Waiting _          -> "waiting..."

--
-- HIGH LEVEL
--

data HurtleF t t' e i a
    = Enqueue i a
    | MakeCall t (t' -> Either e a)

-- | Kick off a new request.
request :: i -> Hurtle t t' e i ()
request x = Hurtle . liftF $ Enqueue x ()

-- | Make a remote call and give back the response when it arrives.
makeCall :: t -> (t' -> Either e a) -> Hurtle t t' e i a
makeCall x = Hurtle . liftF . MakeCall x

instance Functor (HurtleF t t' e i) where
    fmap f (Enqueue x k) = Enqueue x (f k)
    fmap f (MakeCall x k) = MakeCall x (fmap f . k)

newtype Hurtle t t' e i a
    = Hurtle{ unHurtle :: Free (HurtleF t t' e i) a }
      deriving (Functor, Applicative, Monad)

runRequest :: (i -> Hurtle t t' e i a) -> i -> LL t t' e i a
runRequest r = LL . go . unHurtle . r
  where
    go (FreeT (Identity (Pure x))) = FreeT (return (Pure x))
    go (FreeT (Identity (Free (Enqueue x k)))) = FreeT $ do
        tell [x]
        runFreeT $ go k
    go (FreeT (Identity (Free (MakeCall x k)))) = FreeT $
        return (Free (LLF x (either goErr go . k)))
      where
        goErr = FreeT . return . Free . Throw

--
-- LOW LEVEL
--

data Queued a = Queued CallId a

data SomeRequest t t' e i a = SR (t' -> LL t t' e i a) t

data LLF t t' e a
    = LLF t (t' -> a)
    | Throw e

instance Functor (LLF t t' e) where
    fmap g (LLF p f) = LLF p (g . f)
    fmap _ (Throw e) = Throw e

newtype LL t t' e i a = LL (FreeT (LLF t t' e) (WriterT [i] IO) a)

data LLState st t t' e i a = LLState
    { _llNextId   :: Integer
    , _llState    :: st
    , _llQueue    :: Seq.Seq (Queued i)
    , _llInFlight :: Hash.HashMap CallId (SomeRequest t t' e i a)
    }
makeLenses ''LLState

-- Enqueue a new initial state for processing.
llEnqueue :: (Functor m, Monad m)
          => i -> StateT (LLState st t t' e i a) m CallId
llEnqueue x = do
    st <- get
    let st' = st & llNextId +~ 1
                 & llQueue  %~ (Queued (CallId (st ^. llNextId)) x <|)
    CallId (st ^. llNextId) <$ put st'


-- Pull the next initial state from the queue.
llUnqueue :: (Functor m, Monad m)
          => StateT (LLState st t t' e i a) m (Maybe (CallId, i))
llUnqueue = do
    st <- get
    case Seq.viewr (st ^. llQueue) of
        EmptyR -> return Nothing
        queue :> Queued cid x -> Just (cid, x) <$ put (st & llQueue .~ queue)

-- Mark a request as being sent.
llSent :: (Functor m, Monad m)
       => CallId -> t -> (t' -> LL t t' e i a)
       -> StateT (LLState st t t' e i a) m ()
llSent cid t f = do
    st <- get
    put $ st & llInFlight %~ Hash.insert cid (SR f t)

-- Find a request and remove it from the in-flight set.
llFindRequest :: (Functor m, Monad m)
              => CallId
              -> StateT (LLState st t t' e i a) m
                     (Maybe (SomeRequest t t' e i a))
llFindRequest cid = do
    st <- get
    case st ^. llInFlight . at cid of
        Nothing -> return Nothing
        Just sr -> Just sr <$ put (st & llInFlight %~ Hash.delete cid)

runHurtle :: Config t t' e              -- ^ Configuration
          -> (a -> IO ())               -- ^ Final action for each input
          -> (Log e -> IO ())           -- ^ Log handler
          -> [i]                        -- ^ Initial values to enqueue
          -> (i -> Hurtle t t' e i a)   -- ^ Action for each input
          -> IO ()
runHurtle cfg finish logIt xs = runLL cfg finish logIt xs . runRequest

runLL :: Config t t' e              -- ^ Configuration
      -> (a -> IO ())               -- ^ Final action for each input
      -> (Log e -> IO ())           -- ^ Log handler
      -> [i]                        -- ^ Initial values to enqueue
      -> (i -> LL t t' e i a)       -- ^ Action for each input
      -> IO ()
runLL Config{..} finish logIt xs ll  = do
    initialState <- configInit
    stateV <- STM.atomically $
        STM.newTMVar (LLState 0 initialState Seq.empty Hash.empty)
    begun <- STM.atomically newFlagPost
    done  <- STM.atomically newFlagPost

    let withSt = withStateV stateV logIt

    withSt Main $
        forM_ xs $ \x -> do
            cid <- llEnqueue x
            lift . logIt $ Enqueued [cid] Main

    let doStep category cid (LL (FreeT m)) = do
            (resp, enqs) <- runWriterT (hoist lift m)
            cids <- mapM llEnqueue enqs
            lift . logIt $ Enqueued cids category
            case resp of
                Pure x -> lift (finish x)
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
            Just (cid, x) -> do
                withSt category $ doStep category cid (ll x)
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

withStateV :: STM.TMVar (LLState st t t' e i a)
           -> (Log e -> IO ())           -- ^ Log handler
           -> Component
           -> StateT (LLState st t t' e i a) IO c -> IO c
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
