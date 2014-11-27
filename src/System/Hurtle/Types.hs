{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE Safe                       #-}
{-# LANGUAGE TypeFamilies               #-}

module System.Hurtle.Types where

import           Control.Applicative
import           Control.Monad.Trans.Free  (Free)

import qualified System.Hurtle.TypedStore  as TS
import qualified System.Hurtle.TypedStore2 as TS2

-- Cribbed the EqF/OrdF classes from the ShowF package.

class EqF f where
    eqF :: f a -> f b -> Bool

class EqF f => OrdF f where
    compareF :: f a -> f b -> Ordering

newtype WrapEqF f a = WrapEqF (f a)
instance EqF f => Eq (WrapEqF f a) where
    WrapEqF x == WrapEqF y = eqF x y

data BlindEqF f where BlindEqF :: f a -> BlindEqF f
instance EqF f => Eq (BlindEqF f) where
    BlindEqF x == BlindEqF y = eqF x y

newtype WrapOrdF f a = WrapOrdF (f a)
instance OrdF f => Eq (WrapOrdF f a) where
    WrapOrdF x == WrapOrdF y = eqF x y
instance OrdF f => Ord (WrapOrdF f a) where
    compare (WrapOrdF x) (WrapOrdF y) = compareF x y

data BlindOrdF f where BlindOrdF :: f a -> BlindOrdF f
instance OrdF f => Eq (BlindOrdF f) where
    BlindOrdF x == BlindOrdF y = eqF x y
instance OrdF f => Ord (BlindOrdF f) where
    compare (BlindOrdF x) (BlindOrdF y) = compareF x y

class Connection c where
    data InitArgs c :: *
    data Request  c :: * -> *
    data Error    c :: *
    type M        c :: * -> *
    initialise :: InitArgs c -> M c (c i)
    finalise   :: c i -> M c ()
    send       :: OrdF i => c i -> i a -> Request c a -> M c ()
    receive    :: OrdF i => c i -> M c (Response c i)

data Response c i where
    Ok       :: i a -> a -> Response c i
    Retry    :: i a -> Maybe (c i) -> Response c i
    LogError :: Maybe (i a) -> Error c -> Response c i
    Fatal    :: Maybe (i a) -> Error c -> Response c i

data ForkId s (c :: (* -> *) -> *) a
    = ForkId Int
instance EqF (ForkId s c) where
    ForkId i `eqF` ForkId j = i == j
instance OrdF (ForkId s c) where
    ForkId i `compareF` ForkId j = i `compare` j

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
