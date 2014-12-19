{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE Trustworthy      #-}

module System.Hurtle.Unsafe where

import           Control.Applicative
import           Unsafe.Coerce             (unsafeCoerce)

import           System.Hurtle.Common
import qualified System.Hurtle.Log         as Log
import qualified System.Hurtle.TypedStore  as TS
import           System.Hurtle.Types

-- We need to do some nasty coercion stuff here to get the phantom types to
-- match up.
withHurtleState :: (Connection c, Applicative (M c), Monad (M c))
                => InitArgs c
                -> LogFold c l
                -> (forall s. Hurtle s c a)
                -> (forall s. HurtleState s c l -> Hurtle s c a -> M c b)
                -> M c b
withHurtleState args logFold hurtle k =
    TS.typedStore $ \forks -> do
        c <- initialise args
        res <- k (HState Log.firstId c logFold (unsafeCoerce forks))
                 (unsafeCoerce hurtle)
        res <$ finalise c
