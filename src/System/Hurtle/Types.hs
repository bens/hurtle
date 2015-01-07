{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE Safe                       #-}
{-# LANGUAGE TypeFamilies               #-}

module System.Hurtle.Types where

import           Control.Applicative
import           Control.Monad            ((>=>), ap)
import qualified Control.Monad.Par.Class  as Par

import           System.Hurtle.Log        (Log)
import qualified System.Hurtle.Log        as Log
import qualified System.Hurtle.TypedStore as TS

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
    Fatal    :: Maybe (i a) -> Error c -> Response c i

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

-- | The foldl package has quite a few dependencies which we would rather avoid,
-- so by defining 'LogFold' and the 'unLogFold' function we can be compatible
-- with foldl but not depend on it.  See test-src/Main.hs for an example use.
data LogFold c l where
    LogFold :: (x -> Log (Error c) -> M c x) -> M c x -> (x -> M c l)
            -> LogFold c l

unLogFold :: (LogFold c l -> r)
          -> (forall x. (x -> Log (Error c) -> M c x) -> M c x -> (x -> M c l)
                     -> r)
unLogFold g f m k = g (LogFold f m k)

data HurtleState s c l = HState
    { _stNextId   :: Log.Id
    , _stState    :: c (SentRequest s c)
    , _stLogState :: LogFold c l
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
