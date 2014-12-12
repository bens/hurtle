{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Safe                       #-}

module System.Hurtle.Types where

import           Control.Applicative
import           Control.Monad             ((>=>), ap)
import qualified Control.Monad.Par.Class   as Par

import           System.Hurtle.Common
import qualified System.Hurtle.TypedStore  as TS
import qualified System.Hurtle.TypedStore2 as TS2

data ForkId s c a
    = ForkId Int (TS.Id s (SomeProcess s c a))
instance EqF (ForkId s c) where
    ForkId i _ `eqF` ForkId j _ = i == j
instance OrdF (ForkId s c) where
    ForkId i _ `compareF` ForkId j _ = i `compare` j

data SentRequest s t c a where
    SR :: ForkId s c b -> TS2.Id t (SomeRequest s c b a) -> SentRequest s t c a
instance EqF (SentRequest s t c) where
    SR i _ `eqF` SR j _ = BlindEqF i == BlindEqF j
instance OrdF (SentRequest s t c) where
    SR i _ `compareF` SR j _ = BlindOrdF i `compare` BlindOrdF j

data BlockedProcess s c a where
    BP :: ForkId s c b -> (a -> Hurtle s c b) -> BlockedProcess s c a

data SomeProcess s c a
    = ProcessDone a
    | ProcessFailed (Error c)
    | ProcessRunning [BlockedProcess s c a]

data SomeRequest s c b a =
    Req (Request c a) (a -> Hurtle s c b)

data HurtleState s t c = HState
    { _stNextId   :: Int
    , _stState    :: c (SentRequest s t c)
    , _stForks    :: TS.TypedStore s TS.Mono (SomeProcess s c)
    , _stInFlight :: TS2.TypedStore t TS2.NonMono (SomeRequest s c)
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

instance Connection c => Par.ParFuture (Hurtle s c) (Hurtle s c) where
    spawn_ = fork
    get = id

-- | Fork a new process and return an action that will wait for the result.
fork :: Connection c => Hurtle s c a -> Hurtle s c (Hurtle s c a)
fork h = Fork h $ \fid -> Block fid (return . return)

-- | Make a request and wait for the response.
request :: Connection c => Request c a -> Hurtle s c a
request req = Call req return
