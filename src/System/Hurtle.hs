{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module System.Hurtle
  ( CallId, Config(..)
  , Request, Response(..)
  , LogMessage(..), LogLevel(..)
  , makeCall, request
  , runHurtle
  ) where

import           Control.Applicative
import qualified Control.Concurrent         as Conc
import qualified Control.Concurrent.Async   as Async
import qualified Control.Concurrent.STM     as STM
import qualified Control.Exception          as Exc
import           Control.Lens               hiding ((<|))
import           Control.Monad              (forever, guard, when)
import           Control.Monad.Morph        (hoist, lift)
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State  hiding (state)
import           Control.Monad.Trans.Writer
import           Data.Foldable              (mapM_)
import           Data.Hashable              (Hashable)
import qualified Data.HashMap.Strict        as Hash
import           Data.Sequence              (ViewR (..), (<|))
import qualified Data.Sequence              as Seq
import           Text.Printf

import           Prelude                    hiding (mapM_)

newtype CallId
    = CallId Integer deriving (Eq, Show, Hashable)

data Config st t = Config
    { configInit :: IO st
    , configTerm :: st -> IO ()
    , configSend :: st -> CallId -> t -> IO ()
    , configRecv :: st -> IO (Response st t)
    }

data Response st t
    = Ok CallId t
    | Retry CallId (Maybe st)
    | LogError String
    | Fatal String

data LogLevel
    = Debug | Info | Warning | Error deriving (Eq, Ord, Show)

data LogMessage
    = LogMessage{ logLevel   :: LogLevel
                , logSection :: String
                , logMessage :: String
                }
      deriving Show

--
-- HIGH LEVEL
--

data RequestF t e i a
    = Enqueue i a
    | MakeCall t (t -> Either e a)

-- | Initiate a new request.
request :: i -> Request t e i ()
request x = Request . liftF $ Enqueue x ()

-- | Make a remote call and give back the response when it arrives.
makeCall :: t -> (t -> Either e a) -> Request t e i a
makeCall x = Request . liftF . MakeCall x

instance Functor (RequestF t e i) where
    fmap f (Enqueue x k) = Enqueue x (f k)
    fmap f (MakeCall x k) = MakeCall x (fmap f . k)

newtype Request t e i a
    = Request{ unRequest :: Free (RequestF t e i) a }
      deriving (Functor, Applicative, Monad)

runRequest :: (i -> Request t e i a) -> i -> LL st t e i a
runRequest r = LL . go . unRequest . r
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

data SomeRequest st t e i a = SR (t -> LL st t e i a) t

someRequestCont :: Lens' (SomeRequest st t e i a) (t -> LL st t e i a)
someRequestCont = lens (\(SR kont _) -> kont) (\(SR _ t) kont -> SR kont t)

someRequestValue :: Lens' (SomeRequest st t e i a) t
someRequestValue = lens (\(SR _ t) -> t) (\(SR kont _) t -> SR kont t)

data LLF st t e a
    = LLF t (t -> a)
    | Throw e

instance Functor (LLF st t e) where
    fmap g (LLF p f) = LLF p (g . f)

newtype LL st t e i a = LL (FreeT (LLF st t e) (WriterT [i] IO) a)

data LLState st t e i a = LLState
    { _llNextId   :: Integer
    , _llState    :: st
    , _llQueue    :: Seq.Seq (Queued i)
    , _llInFlight :: Hash.HashMap CallId (SomeRequest st t e i a)
    }
makeLenses ''LLState

-- Enqueue a new initial state for processing.
llEnqueue :: (Functor m, Monad m)
          => i -> StateT (LLState st t e i a) m CallId
llEnqueue x = do
    st <- get
    let st' = st & llNextId +~ 1
                 & llQueue  %~ (Queued (CallId (st ^. llNextId)) x <|)
    CallId (st ^. llNextId) <$ put st'


-- Pull the next initial state from the queue.
llUnqueue :: (Functor m, Monad m)
          => StateT (LLState st t e i a) m (Maybe (CallId, i))
llUnqueue = do
    st <- get
    case Seq.viewr (st ^. llQueue) of
        EmptyR -> return Nothing
        queue :> Queued cid x -> Just (cid, x) <$ put (st & llQueue .~ queue)

-- Mark a request as being sent.
llSent :: (Functor m, Monad m)
       => CallId -> t -> (t -> LL st t e i a)
       -> StateT (LLState st t e i a) m ()
llSent cid t f = do
    st <- get
    put $ st & llInFlight %~ Hash.insert cid (SR f t)

-- Find a request and remove it from the in-flight set.
llFindRequest :: (Functor m, Monad m)
              => CallId
              -> StateT (LLState st t e i a) m (Maybe (SomeRequest st t e i a))
llFindRequest cid = do
    st <- get
    case st ^. llInFlight . at cid of
        Nothing -> return Nothing
        Just sr -> Just sr <$ put (st & llInFlight %~ Hash.delete cid)

runHurtle :: Show e
          => Config st t             -- ^ Configuration
          -> (i -> Request t e i a)  -- ^ Action for each input
          -> (LogMessage -> IO ())   -- ^ Log handler
          -> (st -> a -> IO ())      -- ^ Final action for each input
          -> IO (i -> IO (), IO ())  -- ^ Provide an enqueue and a wait action.
runHurtle cfg hl = runLL cfg (runRequest hl)

runLL :: Show e
      => Config st t                -- ^ Configuration
      -> (i -> LL st t e i a)       -- ^ Action for each input
      -> (LogMessage -> IO ())      -- ^ Log handler
      -> (st -> a -> IO ())         -- ^ Final action for each input
      -> IO (i -> IO (), IO ())     -- ^ Provide an enqueue and a wait action.
runLL Config{..} ll logIt finish = do
    initialState <- configInit
    stateV <- STM.atomically $
        STM.newTMVar (LLState 0 initialState Seq.empty Hash.empty)
    begun <- STM.atomically newFlagPost

    let withSt cat x = withStateV stateV logIt cat (x :: Maybe CallId)
        debugL   = (logIt .) . LogMessage Debug
        infoL    = (logIt .) . LogMessage Info
        warningL = (logIt .) . LogMessage Warning
        errorL   = (logIt .) . LogMessage Error

        -- Compute another stage and send out the next request or finish.
        enqueue x = withSt "runLL.enqueue" Nothing $ do
            cid <- llEnqueue x
            lift . debugL "runLL.enqueue" $ "enqueued " ++ show cid

        doStep category cid (LL (FreeT m)) = do
            (resp, enqs) <- runWriterT (hoist lift m)
            cids <- mapM llEnqueue enqs
            lift $ debugL category $ "enqueued " ++ show cids
            st <- use llState
            case resp of
                Pure x -> lift (finish st x)
                Free (LLF t kont) -> do
                    lift (configSend st cid t)
                    llSent cid t (LL . kont)
                Free (Throw e) ->
                    lift $ errorL category (show e)

    -- Launch each enqueued chain.
    sender <- Async.async . forever $ do
        let category = "runLL.sender"
        debugL category "waiting..."
        (cid, x) <- STM.atomically $ do
            (xM, state) <- runState llUnqueue <$> STM.takeTMVar stateV
            maybe STM.retry (\x -> x <$ STM.putTMVar stateV state) xM
        withSt category (Just cid) $ doStep category cid (ll x)
        STM.atomically $ flagPost begun

    -- Receive a reply and run the next step of a chain.
    receiver <- Async.async . forever $ do
        let category = "runLL.receiver"
            onRequest cid reqM = case reqM of
                Nothing -> lift $ errorL category "request handler not found!"
                Just (SR kont t) -> doStep category cid (kont t)
        st <- STM.atomically $ do
            waitFlagPost begun
            state <- STM.readTMVar stateV
            let inFlight = state ^. llInFlight
                queue    = state ^. llQueue
            when (Hash.null inFlight && not (Seq.null queue)) STM.retry
            return (state ^. llState)
        resp <- configRecv st
        withSt category Nothing $ case resp of
            Ok cid t -> do
                reqM <- llFindRequest cid
                onRequest cid ((someRequestValue .~ t) <$> reqM)
            Retry cid stM -> do
                lift $ warningL category ("retrying " ++ show cid)
                mapM_ (llState .=) stM
                llFindRequest cid >>= onRequest cid
            LogError msg ->
                -- TODO: what else??
                lift $ errorL category msg
            Fatal msg ->
                lift $ errorL category msg
        Conc.yield

    let wait = do
            debugL "runLL.wait" "waiting..."
            STM.atomically $ do
                waitFlagPost begun
                state <- STM.readTMVar stateV
                guard (Seq.null (state ^. llQueue))
                guard (Hash.null  (state ^. llInFlight))
            debugL "runLL.wait" "killing threads"
            Async.cancel sender
            Async.cancel receiver

    -- TODO: Async.link the threads but don't bork on ThreadKilled exceptions.
    -- traverse_ Async.link [sender, receiver]
    return (enqueue, wait)


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

withStateV :: Show b
           => STM.TMVar (LLState st t e i a)
           -> (LogMessage -> IO ())      -- ^ Log handler
           -> String -> Maybe b
           -> StateT (LLState st t e i a) IO c -> IO c
withStateV stateV logIt cat xM m = do
    state <- STM.atomically $ STM.takeTMVar stateV
    let cleanup = do
            logIt (LogMessage Warning cat "reverting state")
            STM.atomically (STM.putTMVar stateV state)
    flip Exc.onException cleanup $ do
        logIt . LogMessage Debug cat $
            ">>>" ++ maybe "" (printf " (%s)" . show) xM
        (x, state') <- runStateT m state
        logIt . LogMessage Debug cat $
            "<<<" ++ maybe "" (printf " (%s)" . show) xM
        x <$ STM.atomically (STM.putTMVar stateV state')
