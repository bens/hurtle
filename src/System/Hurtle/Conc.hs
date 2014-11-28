{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

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

module System.Hurtle.Conc
  ( Hurtle, runHurtle, fork, request
    -- * Connections
  , Connection(..), Response(..)
    -- * Contents-oblivious comparisons
  , EqF(..), WrapEqF(..), BlindEqF(..)
  , OrdF(..), WrapOrdF(..), BlindOrdF(..)
  ) where

import           Control.Applicative
import qualified Control.Monad.Trans.Free  as Free

import           System.Hurtle.Common
import           System.Hurtle.Log
import           System.Hurtle.Types

-- | Fork a new process and return an action that will wait for the result.
fork :: Connection c => Hurtle s c a -> Hurtle s c (Hurtle s c a)
fork h = Hurtle . Free.liftF . ForkF (unHurtle h) $ \fid ->
    Hurtle . Free.liftF $ BlockF fid id

-- | Make a request and wait for the response.
request :: Connection c => Request c a -> Hurtle s c a
request req = Hurtle $ Free.liftF (CallF req id)

runHurtle :: (Connection c, Applicative (M c), Monad (M c))
          => InitArgs c                                      -- ^ Initialisation
          -> (forall i. Show i => Log (Error c) i -> M c ()) -- ^ Log handler
          -> (forall s. Hurtle s c a)                        -- ^ Action to run
          -> M c (Either (Error c) a)
runHurtle args logIt' (Hurtle m) = undefined
