{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module System.Hurtle
  ( CallId, Config(..)
  , Request, Response(..)
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
import qualified System.Log.Logger          as Log
import           Text.Printf

import           Prelude                    hiding (mapM_)

newtype CallId
    = CallId Integer deriving (Eq, Show, Hashable)

data Config st a = Config
    { configInit :: IO st
    , configTerm :: st -> IO ()
    , configSend :: st -> CallId -> a -> IO ()
    , configRecv :: st -> IO (Response st a)
    }

data Response st t
    = Ok CallId t
    | Retry CallId (Maybe st)
    | LogError String
    | Fatal String

--
-- HIGH LEVEL
--

data RequestF t i a
    = Enqueue i a
    | MakeCall t (t -> a)

-- | Initiate a new request.
request :: i -> Request t i ()
request x = Request . liftF $ Enqueue x ()

-- | Make a remote call and give back the response when it arrives.
makeCall :: t -> Request t i t
makeCall x = Request . liftF $ MakeCall x id

instance Functor (RequestF t i) where
    fmap f (Enqueue x k) = Enqueue x (f k)
    fmap f (MakeCall x k) = MakeCall x (f . k)

newtype Request t i a
    = Request{ unRequest :: Free (RequestF t i) a }
      deriving (Functor, Applicative, Monad)

runRequest :: (i -> Request t i a) -> i -> LL st t i a
runRequest r = LL . go . unRequest . r
  where
    go (FreeT (Identity (Pure x))) = FreeT (return (Pure x))
    go (FreeT (Identity (Free (Enqueue x k)))) = FreeT $ do
        tell [x]
        runFreeT $ go k
    go (FreeT (Identity (Free (MakeCall x k)))) = FreeT $
        lift $ return (Free (LLF x (go . k)))

--
-- LOW LEVEL
--

data Queued a = Queued CallId a

data SomeRequest st t a b = SR (t -> LL st t a b) t

data LLF st t a = LLF t (t -> a)

instance Functor (LLF st t) where
    fmap g (LLF p f) = LLF p (g . f)

newtype LL st t a b = LL (FreeT (LLF st t) (WriterT [a] IO) b)

data LLState st t a b = LLState
    { _llNextId   :: Integer
    , _llState    :: st
    , _llQueue    :: Seq.Seq (Queued a)
    , _llInFlight :: Hash.HashMap CallId (SomeRequest st t a b)
    }
makeLenses ''LLState

-- Enqueue a new initial state for processing.
llEnqueue :: (Functor m, Monad m)
          => a -> StateT (LLState st t a b) m CallId
llEnqueue x = do
    st <- get
    let st' = st & llNextId +~ 1
                 & llQueue  %~ (Queued (CallId (st ^. llNextId)) x <|)
    CallId (st ^. llNextId) <$ put st'


-- Pull the next initial state from the queue.
llUnqueue :: (Functor m, Monad m)
          => StateT (LLState st t a b) m (Maybe (CallId, a))
llUnqueue = do
    st <- get
    case Seq.viewr (st ^. llQueue) of
        EmptyR -> return Nothing
        queue :> Queued cid x -> Just (cid, x) <$ put (st & llQueue .~ queue)

-- Mark a request as being sent.
llSent :: (Functor m, Monad m)
       => CallId -> t -> (t -> LL st t a b)
       -> StateT (LLState st t a b) m ()
llSent cid t f = do
    st <- get
    put $ st & llInFlight %~ Hash.insert cid (SR f t)

-- Find a request and remove it from the in-flight set.
llFindRequest :: (Functor m, Monad m)
              => CallId
              -> StateT (LLState st t a b) m (Maybe (SomeRequest st t a b))
llFindRequest cid = do
    st <- get
    case st ^. llInFlight . at cid of
        Nothing -> return Nothing
        Just sr -> Just sr <$ put (st & llInFlight %~ Hash.delete cid)

runHurtle :: Config st t             -- ^ Configuration
          -> (i -> Request t i a)    -- ^ Action for each input
          -> (String -> IO ())       -- ^ Error log action
          -> (st -> a -> IO ())      -- ^ Final action for each input
          -> IO (i -> IO (), IO ())  -- ^ Provide an enqueue and a wait action.
runHurtle cfg hl = runLL cfg (runRequest hl)

runLL :: Config st t                -- ^ Configuration
      -> (a -> LL st t a b)         -- ^ Action for each input
      -> (String -> IO ())          -- ^ Error log action
      -> (st -> b -> IO ())         -- ^ Final action for each input
      -> IO (a -> IO (), IO ())     -- ^ Provide an enqueue and a wait action.
runLL Config{..} ll logError finish = do
    initialState <- configInit
    stateV <- STM.atomically $
        STM.newTMVar (LLState 0 initialState Seq.empty Hash.empty)
    begun <- STM.atomically newFlagPost

    let -- Compute another stage and send out the next request or finish.
        enqueue x = withStateV stateV "runLL.enqueue" (Nothing :: Maybe ()) $ do
            cid <- llEnqueue x
            lift $ Log.debugM "runLL.enqueue" $ "enqueued " ++ show cid

        doStep category cid (LL (FreeT m)) = do
            (resp, enqs) <- runWriterT (hoist lift m)
            cids <- mapM llEnqueue enqs
            lift $ Log.debugM category $ "enqueued " ++ show cids
            st <- use llState
            case resp of
                Pure x -> lift (finish st x)
                Free (LLF t kont) -> do
                    lift (configSend st cid t)
                    llSent cid t (LL . kont)

    -- Launch each enqueued chain.
    sender <- Async.async . forever $ do
        let category = "runLL.sender"
        Log.debugM category "waiting..."
        (cid, x) <- STM.atomically $ do
            (xM, state) <- runState llUnqueue <$> STM.takeTMVar stateV
            maybe STM.retry (\x -> x <$ STM.putTMVar stateV state) xM
        withStateV stateV category (Just cid) $
            doStep category cid (ll x)
        STM.atomically $ flagPost begun

    -- Receive a reply and run the next step of a chain.
    receiver <- Async.async . forever $ do
        let category = "runLL.receiver"
        st <- STM.atomically $ do
            waitFlagPost begun
            state <- STM.readTMVar stateV
            let inFlight = state ^. llInFlight
                queue    = state ^. llQueue
            when (Hash.null inFlight && not (Seq.null queue)) STM.retry
            return (state ^. llState)
        resp <- configRecv st
        withStateV stateV category (Nothing :: Maybe ()) $ case resp of
            Ok cid t -> do
                reqM <- llFindRequest cid
                case reqM of
                    Nothing ->
                        lift $ Log.errorM category "request handler not found!"
                    Just (SR kont _) ->
                        doStep category cid (kont t)
            Retry cid stM -> do
                lift $ Log.warningM category ("retrying " ++ show cid)
                mapM_ (llState .=) stM
                reqM <- llFindRequest cid
                case reqM of
                    Nothing ->
                        lift $ Log.errorM category "request handler not found!"
                    Just (SR kont t) ->
                        doStep category cid (kont t)
            LogError msg ->
                -- TODO: what else??
                lift $ logError msg
            Fatal msg ->
                lift $ Log.criticalM category msg
        Conc.yield

    let wait = do
            Log.debugM "runLL.wait" "waiting..."
            STM.atomically $ do
                waitFlagPost begun
                state <- STM.readTMVar stateV
                guard (Seq.null (state ^. llQueue))
                guard (Hash.null  (state ^. llInFlight))
            Log.debugM "runLL.wait" "killing threads"
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

withStateV :: Show x
           => STM.TMVar (LLState st t a b) -> String -> Maybe x
           -> StateT (LLState st t a b) IO c -> IO c
withStateV stateV cat xM m = do
    state <- STM.atomically $ STM.takeTMVar stateV
    let cleanup = do
            Log.warningM cat "reverting state"
            STM.atomically (STM.putTMVar stateV state)
    flip Exc.onException cleanup $ do
        Log.debugM cat $ ">>>" ++ maybe "" (printf " (%s)" . show) xM
        (x, state') <- runStateT m state
        Log.debugM cat $ "<<<" ++ maybe "" (printf " (%s)" . show) xM
        x <$ STM.atomically (STM.putTMVar stateV state')
