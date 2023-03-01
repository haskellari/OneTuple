{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >=702
{-# LANGUAGE DeriveGeneric #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}
#endif

#if __GLASGOW_HASKELL__ >=704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy #-}
#endif

-- | 'Solo' fills the /tuple gap/ with a singleton tuple.
--
-- 'Solo' /does not support/ the usual parenthesized tuple syntax.
--
-- 'Solo'
--
--   * has the expected laziness properties
--
--   * can be pattern-matched
--
--   * ships with instances for several standard type classes,
--     including all those supported by H98-standard tuples
--
--   * requires no language extensions, except for hierarchical modules
--
-- Note: on GHC-9.0 'getSolo' is not a record selector.

module Data.Tuple.Solo (
#if __GLASGOW_HASKELL__ >= 800
    Solo(MkSolo,Solo),
#elif __GLASGOW_HASKELL__ >= 708
    Solo(MkSolo),
    pattern Solo,
#else
    Solo(MkSolo),
#endif
    getSolo,
) where

#ifdef MIN_VERSION_base_orphans
import Data.Orphans ()
#endif

#if MIN_VERSION_base(4,18,0)
import GHC.Tuple (Solo (MkSolo, Solo), getSolo)


#elif MIN_VERSION_base(4,16,0)
import GHC.Tuple (Solo (Solo), getSolo)

pattern MkSolo :: a -> Solo a
pattern MkSolo a = Solo a

{-# COMPLETE MkSolo #-}

#elif MIN_VERSION_base(4,15,0)
import GHC.Tuple (Solo (Solo))

-- | The 'getSolo' function extracts the Solo's getSolo member.
getSolo :: Solo a -> a
getSolo (Solo x) = x

pattern MkSolo :: a -> Solo a
pattern MkSolo a = Solo a

{-# COMPLETE MkSolo #-}

#else

#if MIN_VERSION_base(4,9,0)
#define LIFTED_FUNCTOR_CLASSES 1
#else
#if MIN_VERSION_transformers(0,5,0)
#define LIFTED_FUNCTOR_CLASSES 1
#else
#ifdef MIN_VERSION_transformers_compat
#if MIN_VERSION_transformers_compat(0,5,0) && !(MIN_VERSION_transformers(0,4,0))
#define LIFTED_FUNCTOR_CLASSES 1
#endif
#endif
#endif
#endif

import Control.Applicative (Applicative (..))
import Control.Monad       (ap)
import Control.Monad.Fix   (MonadFix (..))
import Data.Data           (Data)
import Data.Foldable       (Foldable (..))
import Data.Ix             (Ix (..))
import Data.List.NonEmpty  (NonEmpty (..))
import Data.Monoid         (Monoid (..))
import Data.Semigroup      (Semigroup (..))
import Data.Traversable    (Traversable (..))
import Data.Typeable       (Typeable)

import qualified Data.Foldable1 as F1

import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..), Read1 (..))

#if !(MIN_VERSION_base(4,15,0))
import Data.Hashable        (Hashable (..))
import Data.Hashable.Lifted (Hashable1 (..), hashWithSalt1)
#endif

#if LIFTED_FUNCTOR_CLASSES
#if MIN_VERSION_base(4,10,0)
import Data.Functor.Classes (readData, readUnaryWith, liftReadListDefault, liftReadListPrecDefault)
#else
import Data.Functor.Classes (readsData, readsUnaryWith)
#endif
#endif

#if MIN_VERSION_base(4,4,0)
import GHC.Generics        (Generic, Generic1)
import Control.Monad.Zip   (MonadZip (..))
#endif

-- | Solo is the singleton tuple data type.
data Solo a = MkSolo { getSolo :: a }
  deriving
    ( Eq,Ord,Bounded,Read,Typeable,Data
#if MIN_VERSION_base(4,4,0)
    , Generic
#if __GLASGOW_HASKELL__ >=706
    , Generic1
#endif
#endif
    )

#if __GLASGOW_HASKELL__ >= 708
#if __GLASGOW_HASKELL__ >= 710
pattern Solo :: a -> Solo a
#endif
pattern Solo a = MkSolo a
#endif

#if __GLASGOW_HASKELL__ >= 800
{-# COMPLETE Solo #-}
#endif


instance Show a => Show (Solo a) where
  showsPrec d (MkSolo x) = showParen (d > 10) $
      showString "MkSolo " . showsPrec 11 x

instance (Enum a) => Enum (Solo a) where
    succ = fmap succ
    pred = fmap pred
    toEnum = pure . toEnum
    fromEnum (MkSolo x) = fromEnum x

instance (Ix a) => Ix (Solo a) where
    range   (MkSolo x, MkSolo y) = map MkSolo (range (x,y))
    index   (MkSolo x, MkSolo y) (MkSolo z) = index   (x,y) z
    inRange (MkSolo x, MkSolo y) (MkSolo z) = inRange (x,y) z

instance Foldable Solo where
    fold (MkSolo m) = m
    foldMap f (MkSolo x) = f x
    foldr f b (MkSolo x) = f x b
    foldl f a (MkSolo x) = f a x
    foldr1 _f (MkSolo x) = x
    foldl1 _f (MkSolo x) = x

    -- TODO: add rest of the methods
#if MIN_VERSION_base(4,8,0)
    null _ = False
    length _ = 1

    maximum = getSolo
    minimum = getSolo
    sum     = getSolo
    product = getSolo

    toList (MkSolo a) = [a]
#endif

-- | @since 0.4
instance F1.Foldable1 Solo where
    foldMap1 f (MkSolo y) = f y
    toNonEmpty (MkSolo x) = x :| []
    minimum (MkSolo x) = x
    maximum (MkSolo x) = x
    head (MkSolo x) = x
    last (MkSolo x) = x

instance Traversable Solo where
    traverse f (MkSolo x) = fmap MkSolo (f x)
    sequenceA (MkSolo x) = fmap MkSolo x

instance Functor Solo where
    fmap f (MkSolo x) = MkSolo (f x)

instance Applicative Solo where
    pure = MkSolo

    MkSolo f <*> MkSolo x = MkSolo (f x)
    _ *> x = x
    x <* _ = x

#if MIN_VERSION_base(4,10,0)
    liftA2 f (Solo x) (Solo y) = Solo (f x y)
#endif

instance Monad Solo where
    return = pure
    (>>) = (*>)
    MkSolo x >>= f = f x

instance Semigroup a => Semigroup (Solo a) where
    MkSolo x <> MkSolo y = MkSolo (x <> y)

instance Monoid a => Monoid (Solo a) where
    mempty = MkSolo mempty
    mappend (MkSolo x) (MkSolo y) = MkSolo (mappend x y)

instance MonadFix Solo where
    mfix f = let a = f (getSolo a) in a

#if MIN_VERSION_base(4,4,0)
instance MonadZip Solo where
    mzipWith f (MkSolo a) (MkSolo b) = MkSolo (f a b)
#endif

#ifdef LIFTED_FUNCTOR_CLASSES
instance Eq1 Solo where
  liftEq eq (MkSolo a) (MkSolo b) = a `eq` b

instance Ord1 Solo where
  liftCompare cmp (MkSolo a) (MkSolo b) = cmp a b

instance Read1 Solo where
#if MIN_VERSION_base(4,10,0)
    liftReadPrec rp _ = readData (readUnaryWith rp "MkSolo" MkSolo)

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault
#else
    liftReadsPrec rp _ = readsData $ readsUnaryWith rp "MkSolo" MkSolo
#endif

instance Show1 Solo where
    liftShowsPrec sp _ d (MkSolo x) = showParen (d > 10) $
      showString "MkSolo " . sp 11 x

#else
instance Eq1 Solo where eq1 = (==)
instance Ord1 Solo where compare1 = compare
instance Read1 Solo where readsPrec1 = readsPrec
instance Show1 Solo where showsPrec1 = showsPrec
#endif

#endif

#if !(MIN_VERSION_base(4,15,0))
-- | @since 0.3.1
instance Hashable a => Hashable (Solo a) where
    hashWithSalt = hashWithSalt1

-- | @since 0.3.1
instance Hashable1 Solo where
    liftHashWithSalt h salt (MkSolo a) = h salt a
#endif
