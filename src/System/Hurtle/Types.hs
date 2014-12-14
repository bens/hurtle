{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Safe                       #-}

module System.Hurtle.Types where

import           Control.Applicative
import           Control.Foldl
import           Control.Monad            ((>=>), ap)
import qualified Control.Monad.Par.Class  as Par

import           System.Hurtle.Common
import           System.Hurtle.Log        (Log)
import qualified System.Hurtle.Log        as Log
import qualified System.Hurtle.TypedStore as TS

data ForkId s c a
    = ForkId Log.Id (TS.Id s (Process s c a))
instance EqF (ForkId s c) where
    ForkId i _ `eqF` ForkId j _ = i == j
instance OrdF (ForkId s c) where
    ForkId i _ `compareF` ForkId j _ = i `compare` j

data SentRequest s c a where
    SR :: ForkId s c b -> Request c a -> (a -> Hurtle s c b) -> SentRequest s c a
instance EqF (SentRequest s c) where
    SR i _ _ `eqF` SR j _ _ = BlindEqF i == BlindEqF j
instance OrdF (SentRequest s c) where
    SR i _ _ `compareF` SR j _ _ = BlindOrdF i `compare` BlindOrdF j

data BlockedProcess s c a where
    BP :: ForkId s c b -> (a -> Hurtle s c b) -> BlockedProcess s c a

data Process s c a
    = ProcessDone a
    | ProcessFailed (Error c)
    | ProcessRunning [BlockedProcess s c a]

data HurtleState s c l = HState
    { _stNextId   :: Log.Id
    , _stState    :: c (SentRequest s c)
    , _stLogState :: FoldM (M c) (Log (Error c)) l
    , _stForks    :: TS.TypedStore s TS.Mono (Process s c)
    }

data Hurtle s c a where
    Done :: a -> Hurtle s c a
    Call :: Request c b -> (b -> Hurtle s c a) -> Hurtle s c a
    Fork :: Hurtle s c b -> (ForkId s c b -> Hurtle s c a) -> Hurtle s c a
    Block :: ForkId s c b -> (b -> Hurtle s c a) -> Hurtle s c a

instance Functor (Hurtle s c) where
    fmap f (Done x)      = Done (f x)
    fmap f (Call req k)  = Call req  (fmap f . k)
    fmap f (Fork m k)    = Fork m    (fmap f . k)
    fmap f (Block fid k) = Block fid (fmap f . k)

instance Applicative (Hurtle s c) where
    pure = Done
    f <*> x = ap f x

instance Monad (Hurtle s c) where
    return = Done
    Done x      >>= f = f x
    Call req k  >>= f = Call req  (k >=> f)
    Fork m k    >>= f = Fork m    (k >=> f)
    Block fid k >>= f = Block fid (k >=> f)

instance Par.ParFuture (Hurtle s c) (Hurtle s c) where
    spawn_ = fork
    get = id

-- | Fork a new process and return an action that will wait for the result.
fork :: Hurtle s c a -> Hurtle s c (Hurtle s c a)
fork h = Fork h $ \fid -> return $ Block fid return

-- | Make a request and wait for the response.
request :: Request c a -> Hurtle s c a
request req = Call req return
