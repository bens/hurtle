{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE Trustworthy      #-}

module System.Hurtle.Unsafe where

import           Control.Applicative
import           Unsafe.Coerce             (unsafeCoerce)

import           System.Hurtle.Common
import qualified System.Hurtle.TypedStore  as TS
import qualified System.Hurtle.TypedStore2 as TS2
import           System.Hurtle.Types

-- We need to do some nasty coercion stuff here to get the phantom types to
-- match up.
withHurtleState :: (Connection c, Applicative (M c), Monad (M c))
                => InitArgs c
                -> (forall s. Hurtle s c a)
                -> (forall s t. HurtleState s t c -> Hurtle s c a -> M c b)
                -> M c b
withHurtleState args hurtle k =
    TS.typedStore' $ \forks ->
        TS2.typedStore $ \inFlight -> do
            c <- initialise args
            res <- k (HState 0 c (unsafeCoerce forks) inFlight)
                     (unsafeCoerce hurtle)
            res <$ finalise c
