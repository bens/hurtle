{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE Safe             #-}

{- |
= Design Overview

== Process an action

  [@Pure@] If this is a fork, put the result in the placeholder, then run each
    of the blocked processes until they block.  If this is the main process, put
    the result in the proper place and go back to waiting for the other
    processes to complete (or just @finish@ the @Connection@ and return?).

  [@Call@] Send the request and put the continuation in a hashmap to await the
    response.

  [@Fork@] Process the first action of the fork (call or another fork) and set
    up a place for the fork to put its result when it's done.  After that first
    step is processed, continue with the other fork of the original process.

  [@Block@] If the fork has not yet completed, register the continuation in the
    store, otherwise just take the finished value and continue.

== When all processes are blocked

Begin waiting for responses to calls, when one arrives it will have a call id if
it succeeded.  Take the response value and apply to the continuation, then
process as above.

If a request fails and returns an error result, propagate the error to the
result storage location and notify blocked processes.

== Forks

The place where a fork puts its result is also where blocked processes can
register themselves.  Some kind of existential will be needed because different
forks will have different types which the continuations all depend on.  When a
fork finishes, its result needs to be kept around in case something blocks on it
after it's finished.

Different processes waiting on the same fork might have different types too, so
their result types need to be wrapped in existentials as well.

Some way of recognising the main thread and its result is needed, as well as
pulling it out and using it as the final result of the computation.

== Example

> test :: Hurtle s Hdfs (Int, String)
> test = do x <- fork $ request (ReqInt 0)
>           y <- fork $ request (ReqStr "foo")
>           (,) <$> x <*> y

-}

module System.Hurtle
  ( Hurtle, runHurtle, fork, request
    -- * Connections
  , Connection(..), Response(..)
    -- * Contents-oblivious comparisons
  , EqF(..), WrapEqF(..), BlindEqF(..)
  , OrdF(..), WrapOrdF(..), BlindOrdF(..)
    -- * Logging
  , module System.Hurtle.Log
  ) where

import           Control.Applicative
import           Control.Monad             (forM_)
import           Control.Monad.Fix         (fix)
import           Control.Monad.Trans.Free  (Free, FreeT(..), FreeF(..))
import qualified Control.Monad.Trans.Free  as Free
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class (lift)
import           Data.Functor.Identity
import           Pipes                     ((>->))
import qualified Pipes                     as P

import           System.Hurtle.Common
import           System.Hurtle.Log
import           System.Hurtle.Types
import qualified System.Hurtle.TypedStore  as TS
import qualified System.Hurtle.TypedStore2 as TS2
import           System.Hurtle.Unsafe

-- | Fork a new process and return an action that will wait for the result.
fork :: Connection c => Hurtle s c a -> Hurtle s c (Hurtle s c a)
fork h = Hurtle . Free.liftF . ForkF (unHurtle h) $ \fid ->
    Hurtle . Free.liftF $ BlockF fid id

-- | Make a request and wait for the response.
request :: Connection c => Request c a -> Hurtle s c a
request req = Hurtle $ Free.liftF (CallF req id)

sendingRequest :: (Functor m, Monad m)
               => SomeRequest s c b a
               -> StateT (HurtleState s t c) m
                      (TS2.Id t (SomeRequest s c b a))
sendingRequest sr = do
    st <- get
    let (i, inflight') = TS2.insert sr (_stInFlight st)
    i <$ put st{ _stInFlight = inflight' }

forkProcess :: (Functor m, Monad m)
            => StateT (HurtleState s t c) m (ForkId s c a)
forkProcess = do
    st <- get
    let (fid, forks') = TS.insert (ProcessRunning []) (_stForks st)
    put st{ _stNextId = _stNextId st + 1, _stForks = forks' }
    return (ForkId (_stNextId st) fid)

data Step s c where
    Step :: ForkId s c a -> Free (HurtleF s c) a -> Step s c

runHurtle :: (Connection c, Applicative (M c), Monad (M c))
          => InitArgs c                                      -- ^ Initialisation
          -> (forall i. Show i => Log (Error c) i -> M c ()) -- ^ Log handler
          -> (forall s. Hurtle s c a)                        -- ^ Action to run
          -> M c (Either (Error c) a)
runHurtle args logIt' h' = withHurtleState args h' $ \st0 (Hurtle h) -> do
    let logIt x = logIt' x where _ = [Sending (0 :: Int), x]

        unlessDone (ForkId _ fid) k = do
            st <- lift get
            case fid TS.! _stForks st of
                ProcessDone x -> return (Right x)
                ProcessFailed err -> return (Left err)
                ProcessRunning _ -> k

        process (Step forkId@(ForkId cid fid) (FreeT (Identity m))) = case m of
            Pure x -> do
                forks <- gets _stForks
                case fid TS.! forks of
                    ProcessDone _   -> error "wtf?"  -- finished twice?!
                    ProcessFailed _ -> error "wtf?"  -- finished twice?!
                    ProcessRunning bps -> do
                        lift . logIt $ Finished cid
                        let forks' = TS.update fid (ProcessDone x) forks
                        modify $ \st -> st{ _stForks = forks' }
                        -- resume any waiting processes
                        forM_ bps $ \(BP forkId'@(ForkId cid' _) k) -> do
                            lift . logIt $ Continuing cid' cid
                            process (Step forkId' (k x))
            Free (CallF req k) -> do
                lift . logIt $ Sending cid
                callId <- sendingRequest (Req req k)
                st <- get
                lift $ send (_stState st) (SR forkId callId) req
            Free (ForkF m' k) -> do
                forkId'@(ForkId cid' _) <- forkProcess
                lift . logIt $ Forked cid cid'
                process (Step forkId' m')
                process (Step forkId (k forkId'))
            Free (BlockF (ForkId cid' fid') k) -> do
                lift . logIt $ Blocked cid cid'
                forks <- gets _stForks
                case fid' TS.! forks of
                    ProcessDone x -> do
                        lift . logIt $ Continuing cid cid'
                        process (Step forkId (k x))
                    ProcessFailed err -> do
                        lift . logIt $ PropagatingError cid' cid
                        let procFailed = TS.update fid (ProcessFailed err)
                        modify $ \st -> st{ _stForks = procFailed forks }
                    ProcessRunning bps -> do
                        let bp = BP forkId k
                            register = TS.update fid' (ProcessRunning (bp:bps))
                        modify $ \st -> st{ _stForks = register forks }

        receiver rootFid@(ForkId _ fid) = fix $ \loop -> unlessDone rootFid $ do
            response <- lift (gets _stState >>= lift . receive)
            case response of
                Ok (SR forkId@(ForkId cid _) callId) resp -> do
                    st <- lift get
                    case TS2.lookup callId (_stInFlight st) of
                        Nothing -> error "wtf?"
                        Just (Req _ k) -> do
                            lift . lift . logIt $ Resumed cid
                            lift $ put st{
                                _stInFlight = TS2.delete callId (_stInFlight st)
                                }
                            P.yield (Step forkId (k resp))
                Retry (SR forkId@(ForkId cid _) callId) _ -> do
                    lift . lift . logIt $ Retrying cid
                    st <- lift get
                    case TS2.lookup callId (_stInFlight st) of
                        Nothing -> error "wtf?"
                        Just (Req req k') -> do
                            let k = FreeT (Identity (Free (CallF req k')))
                            P.yield (Step forkId k)
                Fatal Nothing err -> do
                    lift . lift . logIt $ SystemError err
                    st <- lift get
                    let failed = TS.update fid (ProcessFailed err)
                    lift $ put st{ _stForks = failed (_stForks st) }
                Fatal (Just (SR (ForkId cid' fid') _)) err -> do
                    lift . lift . logIt $ SystemError' cid' err
                    st <- lift get
                    let failed = TS.update fid' (ProcessFailed err)
                    lift $ put st{ _stForks = failed (_stForks st) }
            loop

    flip evalStateT st0 . P.runEffect $
        let kickoff = do
                forkId <- lift forkProcess
                forkId <$ P.yield (Step forkId h)
        in (kickoff >>= receiver) >-> fix (P.await >>= lift . process >>)
