{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE Safe                       #-}

module System.Hurtle.Types where

import           Control.Applicative
import           Control.Monad.Trans.Free  (Free)

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
    BP :: ForkId s c b -> (a -> Free (HurtleF s c) b) -> BlockedProcess s c a

data SomeProcess s c a
    = ProcessDone a
    | ProcessFailed (Error c)
    | ProcessRunning

data SomeRequest s c b a =
    Req (Request c a) (a -> Free (HurtleF s c) b)

data HurtleState s t c = HState
    { _stNextId   :: Int
    , _stState    :: c (SentRequest s t c)
    , _stForks    :: TS.TypedStore s TS.Mono (SomeProcess s c)
    , _stInFlight :: TS2.TypedStore t TS2.NonMono (SomeRequest s c)
    }

data HurtleF s c a where
    CallF  :: Request c b -> (b -> a) -> HurtleF s c a
    ForkF  :: Free (HurtleF s c) b -> (ForkId s c b -> a) -> HurtleF s c a
    BlockF :: ForkId s c b -> (b -> a) -> HurtleF s c a

instance Functor (HurtleF s c) where
    fmap f (CallF req k)  = CallF req (f . k)
    fmap f (ForkF m k)    = ForkF m (f . k)
    fmap f (BlockF fid k) = BlockF fid (f . k)

newtype Hurtle s c a = Hurtle{ unHurtle :: Free (HurtleF s c) a }
    deriving Functor

instance Applicative (Hurtle s c) where
    pure = Hurtle . pure
    Hurtle f <*> Hurtle x = Hurtle $ f <*> x

instance Monad (Hurtle s c) where
    return = Hurtle . return
    Hurtle m >>= f = Hurtle $ m >>= unHurtle . f
