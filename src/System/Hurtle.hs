{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Hurtle
  ( Hurtle, runHurtle, makeCall, request
    -- * Backends
  , module System.Hurtle.Common
    -- * Logging
  , module System.Hurtle.Log
  ) where

import           Control.Applicative
import           Control.Lens               hiding (Level, (<|))
import           Control.Monad              ((>=>))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Writer (tell)

import           System.Hurtle.Common
import           System.Hurtle.LL
import           System.Hurtle.Log

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
    go m = FreeT $ case runFree m of
        Pure x -> return (Pure x)
        Free (Enqueue x k) -> tell [x] >> runFreeT (go k)
        Free (MakeCall x k) -> return (Free (LLF x (either err go . k)))
    err = FreeT . return . Free . Throw

runHurtle :: Config t t' e              -- ^ Configuration
          -> (a -> IO ())               -- ^ Final action for each input
          -> (Log e -> IO ())           -- ^ Log handler
          -> [i]                        -- ^ Initial values to enqueue
          -> (i -> Hurtle t t' e i a)   -- ^ Action for each input
          -> IO ()
runHurtle cfg finish logIt xs f =
    runLL cfg logIt xs (runRequest f >=> liftIO . finish)
