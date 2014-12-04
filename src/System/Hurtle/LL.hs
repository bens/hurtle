{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}

module System.Hurtle.LL (Forkable(..), LogHandler, LL, runLL, makeCall) where

import           Control.Applicative
import qualified Control.Concurrent         as Conc
import           Control.Monad.Fix          (fix)
import           Control.Monad.Morph        (lift)
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.State  hiding (state)
import           Control.Monad.Trans.Writer
import           Data.Foldable              (forM_, mapM_)
import           Pipes                      ((>->))
import qualified Pipes                      as P

import           Prelude                    hiding (mapM_)

import           System.Hurtle.Common
import           System.Hurtle.Log
import qualified System.Hurtle.TypedStore   as TS

data SomeRequest c r a = SR (a -> LL c r r) (Request c a)

data LLF c a where
    LLF :: Request c b -> (b -> a) -> LLF c a
    Throw :: Error c -> LLF c a

instance Functor (LLF c) where
    fmap f (LLF p k) = LLF p (f . k)
    fmap _ (Throw e) = Throw e

newtype LL c r a =
    LL{ unLL :: FreeT (LLF c) (WriterT [LL c r r] (M c)) a }

deriving instance (Connection c, Monad (M c)) => Functor (LL c r)
deriving instance (Connection c, Monad (M c)) => Applicative (LL c r)
deriving instance (Connection c, Monad (M c)) => Monad (LL c r)

data CallId s c r a
    = CallId Int (TS.Id s (SomeRequest c r a))

instance EqF (CallId s c r) where
    CallId i _ `eqF` CallId j _ = i == j

instance OrdF (CallId s c r) where
    CallId i _ `compareF` CallId j _ = i `compare` j

data CallId' = CallId' Int

instance Show CallId' where
    showsPrec d (CallId' i) =
        showParen (d > 10) $ showString "#" . showsPrec 11 i

data LLState s c i r = LLState
    { _llNextId   :: Int
    , _llState    :: c i
    , _llInFlight :: TS.TypedStore s TS.NonMono (SomeRequest c r)
    }

makeCall :: (Connection c, Monad (M c)) => Request c a -> LL c r a
makeCall req = LL . liftF $ LLF req id

-- Find a request and remove it from the in-flight set.
llFindRequest :: (Connection c, Functor (M c), Monad (M c))
              => TS.Id s (SomeRequest c r a)
              -> StateT (LLState s c i r) (M c) (Maybe (SomeRequest c r a))
llFindRequest tid = do
    st <- get
    case TS.lookup tid (_llInFlight st) of
        Nothing -> return Nothing
        Just sr -> Just sr <$ put st{
            _llInFlight = TS.delete tid (_llInFlight st)
            }

type LogHandler c = forall i. Show i => Log (Error c) i -> M c ()

runLL :: (Connection c, Functor (M c), Monad (M c))
      => InitArgs c        -- ^ Configuration
      -> (a -> M c ())     -- ^ Final action for each input
      -> LogHandler c      -- ^ Log handler
      -> LL c a a
      -> M c ()
runLL initArgs finish logIt ll = TS.typedStore $ \inflight -> do
    let lll = fmap (\(CallId x _) -> CallId' x)
        nextId = do
            st <- get
            _llNextId st <$ put st{ _llNextId = _llNextId st + 1 }
        starter = P.await >>= (lift . go) >> starter
          where
            go (cid, LL (FreeT m)) = do
                lift . logIt $ lll GotLock
                (resp, forks) <- lift (runWriterT m)
                case resp of
                    Pure x -> do
                        forM_ forks $ \m' -> do{ i <- nextId; go (i, m') }
                        lift . logIt $ Finished (CallId' cid)
                        lift $ finish x
                    Free (Throw e) -> lift . logIt . lll $ SystemError e
                    Free (LLF req k) -> do
                        tid <- do
                            s <- get
                            let (tid, inflight') =
                                    TS.insert (SR (LL . k) req) (_llInFlight s)
                            tid <$ put s{ _llInFlight = inflight' }
                        lift . logIt $ Sending (CallId' cid)
                        st <- gets _llState
                        lift $ send st (CallId cid tid) req
                        forM_ forks $ \m' -> do{ i <- nextId; go (i, m') }
                lift . logIt $ lll ReleasedLock

        receiver = fix $ \loop -> do
            lift . lift . logIt $ lll Waiting
            result <- lift (gets _llState >>= lift . receive)
            case result of
                Ok c@(CallId cid tid) resp -> do
                    reqM <- lift $ llFindRequest tid
                    case reqM of
                        Nothing -> lift . lift . logIt . lll $ NoHandlerFound c
                        Just (SR k _) -> P.yield (cid, k resp) >> loop
                Retry c@(CallId _ tid) stM -> do
                    lift . lift . logIt . lll $ Retrying c
                    lift $ mapM_ (\x -> modify $ \s -> s{ _llState = x }) stM
                    reqM <- lift $ llFindRequest tid
                    case reqM of
                        Nothing ->
                            lift . lift . logIt . lll $ NoHandlerFound c
                        Just sr -> lift . modify $ \st -> st{
                            _llInFlight = TS.update tid sr (_llInFlight st)
                            }
                    loop
                Fatal _ e ->
                    lift . lift . logIt . lll $ SystemError e

        kickoff = do{ i <- lift nextId; P.yield (i, ll) }
        program = (kickoff >> receiver) >-> starter

    initialSt <- initialise initArgs
    let initialState = LLState 0 initialSt inflight
    finalState <- execStateT (P.runEffect program) initialState
    finalise (_llState finalState)

--
-- Forkable
--

class Monad m => Forkable m where
    type Final m :: *
    fork :: m (Final m) -> m ()

instance (Connection c, Functor (M c), Monad (M c)) => Forkable (LL c r) where
    type Final (LL c r) = r
    fork m = LL $ FreeT (Pure () <$ tell [m])

instance Forkable IO where
    type Final IO = ()
    fork m = () <$ Conc.forkIO m
