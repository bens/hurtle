{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RankNTypes                 #-}

module System.Hurtle.Common where

data Box f where Box :: f a -> Box f

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
