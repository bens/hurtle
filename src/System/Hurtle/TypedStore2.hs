{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE Trustworthy    #-}

-- | A container for keeping values of various types together.
module System.Hurtle.TypedStore2
  ( Id(), Mode(..), TypedStore()
  , typedStore, typedStore'
  , insert, lookup, (!), update, delete
  ) where

import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IM
import           Data.Maybe           (fromJust)
import           Unsafe.Coerce        (unsafeCoerce)

import           Prelude              hiding (lookup)

import           System.Hurtle.Common (Box2(..))

data Mode
    = Mono     -- ^ Monotonic (no deletes, only inserts and updates)
    | NonMono  -- ^ Non-monotonic (allows deletes)

newtype Id s a = Id Int

unbox :: Box2 f -> f a b
unbox (Box2 x) = unsafeCoerce x

data TypedStore s (mode :: Mode) f = TS Int (IntMap (Box2 f))

insert :: f a b -> TypedStore s mode f -> (Id s (f a b), TypedStore s mode f)
insert x (TS n st) = (Id n, TS (succ n) (IM.insert n (Box2 x) st))

lookup :: Id s (f a b) -> TypedStore s mode f -> Maybe (f a b)
lookup (Id i) (TS _ st) = fmap unbox (IM.lookup i st)

(!) :: Id s (f a b) -> TypedStore s Mono f -> f a b
Id i ! TS _ st = fromJust (fmap unbox (IM.lookup i st))

update :: Id s (f a b) -> f a b -> TypedStore s mode f -> TypedStore s mode f
update (Id i) x (TS n st) = TS n (IM.insert i (Box2 x) st)

delete :: Id s (f a b) -> TypedStore s NonMono f -> TypedStore s NonMono f
delete (Id i) (TS n st) = TS n (IM.delete i st)

-- | Create a 'NonMono'tonic store, that is 'delete's are allowed but you can't
-- use the '!' operator and have to deal with 'Maybe's from 'lookup'.
--
-- Uses an existential phantom type for safety.  The module would be crazy
-- without this protection.
typedStore :: (forall s. TypedStore s NonMono f -> a) -> a
typedStore f = f (TS 0 IM.empty)

-- | Create a 'Mono'tonic store, that is 'delete's are not allowed but you can use
-- the '!' operator and not deal with 'Maybe's.
--
-- Uses an existential phantom type for safety.  The module would be crazy
-- without this protection.
typedStore' :: (forall s. TypedStore s Mono f -> a) -> a
typedStore' f = f (TS 0 IM.empty)
