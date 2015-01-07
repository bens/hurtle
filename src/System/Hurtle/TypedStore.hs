{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE Trustworthy    #-}

-- | A container for keeping values of various types together.
module System.Hurtle.TypedStore
  ( Id(), Mode(..), TypedStore()
  , typedStore, insert, lookup, (!), update, delete
  ) where

import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IM
import           Data.Maybe           (fromJust)
import           Unsafe.Coerce        (unsafeCoerce)

import           Prelude              hiding (lookup)

data Box f where Box :: f a -> Box f

data Mode
    = Mono     -- ^ Monotonic (no deletes, only inserts and updates)
    | NonMono  -- ^ Non-monotonic (allows deletes)

newtype Id s a = Id Int

unbox :: Box f -> f a
unbox (Box x) = unsafeCoerce x

data TypedStore s (mode :: Mode) f = TS Int (IntMap (Box f))

insert :: f a -> TypedStore s mode f -> (Id s (f a), TypedStore s mode f)
insert x (TS n st) = (Id n, TS (succ n) (IM.insert n (Box x) st))

lookup :: Id s (f a) -> TypedStore s mode f -> Maybe (f a)
lookup (Id i) (TS _ st) = fmap unbox (IM.lookup i st)

(!) :: Id s (f a) -> TypedStore s Mono f -> f a
Id i ! TS _ st = fromJust (fmap unbox (IM.lookup i st))

update :: Id s (f a) -> f a -> TypedStore s mode f -> TypedStore s mode f
update (Id i) x (TS n st) = TS n (IM.insert i (Box x) st)

delete :: Id s (f a) -> TypedStore s NonMono f -> TypedStore s NonMono f
delete (Id i) (TS n st) = TS n (IM.delete i st)

-- | Create a 'NonMono'tonic store, that is 'delete's are allowed but you can't
-- use the '!' operator and have to deal with 'Maybe's from 'lookup'.
--
-- Uses an existential phantom type for safety.
typedStore :: (forall s. TypedStore s mode f -> a) -> a
typedStore f = f (TS 0 IM.empty)
