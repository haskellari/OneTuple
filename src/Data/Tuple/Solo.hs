{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >=702
{-# LANGUAGE DeriveGeneric #-}
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
    Solo(Solo),
    getSolo,
) where

#ifdef MIN_VERSION_base_orphans
import Data.Orphans ()
#endif

#if MIN_VERSION_base(4,16,0)
import GHC.Tuple (Solo (Solo), getSolo)

#elif MIN_VERSION_base(4,15,0)
import GHC.Tuple (Solo (Solo))

-- | The 'getSolo' function extracts the Solo's getSolo member.
getSolo :: Solo a -> a
getSolo (Solo x) = x

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
import Data.Monoid         (Monoid (..))
import Data.Semigroup      (Semigroup (..))
import Data.Traversable    (Traversable (..))
import Data.Typeable       (Typeable)

import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..), Read1 (..))

#if LIFTED_FUNCTOR_CLASSES
#if MIN_VERSION_base(4,10,0)
import Data.Functor.Classes (readData, readUnaryWith, liftReadListDefault, liftReadListPrecDefault)
#else
import Data.Functor.Classes (readsData, readsUnaryWith)
#endif
#endif

#if MIN_VERSION_base(4,4,0)
import GHC.Generics        (Generic, Generic1)
#endif

#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip   (MonadZip (..))
#endif

-- | Solo is the singleton tuple data type.
data Solo a = Solo { getSolo :: a }
  deriving
    ( Eq,Ord,Bounded,Read,Typeable,Data
#if MIN_VERSION_base(4,4,0)
    , Generic
#if __GLASGOW_HASKELL__ >=706
    , Generic1
#endif
#endif
    )

instance Show a => Show (Solo a) where
  showsPrec d (Solo x) = showParen (d > 10) $
      showString "Solo " . showsPrec 11 x

instance (Enum a) => Enum (Solo a) where
    succ = fmap succ
    pred = fmap pred
    toEnum = pure . toEnum
    fromEnum (Solo x) = fromEnum x

instance (Ix a) => Ix (Solo a) where
    range   (Solo x, Solo y) = map Solo (range (x,y))
    index   (Solo x, Solo y) (Solo z) = index   (x,y) z
    inRange (Solo x, Solo y) (Solo z) = inRange (x,y) z

instance Foldable Solo where
    fold (Solo m) = m
    foldMap f (Solo x) = f x
    foldr f b (Solo x) = f x b
    foldl f a (Solo x) = f a x
    foldr1 _f (Solo x) = x
    foldl1 _f (Solo x) = x

    -- TODO: add rest of the methods
#if MIN_VERSION_base(4,8,0)
    null _ = False
    length _ = 1

    maximum = getSolo
    minimum = getSolo
    sum     = getSolo
    product = getSolo

    toList (Solo a) = [a]
#endif

instance Traversable Solo where
    traverse f (Solo x) = fmap Solo (f x)
    sequenceA (Solo x) = fmap Solo x


instance Functor Solo where
    fmap f (Solo x) = Solo (f x)

instance Applicative Solo where
    pure = Solo

    Solo f <*> Solo x = Solo (f x)
    _ *> x = x
    x <* _ = x

#if MIN_VERSION_base(4,10,0)
    liftA2 f (Solo x) (Solo y) = Solo (f x y)
#endif

instance Monad Solo where
    return = pure
    (>>) = (*>)
    Solo x >>= f = f x

instance Semigroup a => Semigroup (Solo a) where
    Solo x <> Solo y = Solo (x <> y)

instance Monoid a => Monoid (Solo a) where
    mempty = Solo mempty
    mappend (Solo x) (Solo y) = Solo (mappend x y)

instance MonadFix Solo where
    mfix f = let a = f (getSolo a) in a

#if MIN_VERSION_base(4,4,0)
instance MonadZip Solo where
    mzipWith f (Solo a) (Solo b) = Solo (f a b)
#endif

#ifdef LIFTED_FUNCTOR_CLASSES
instance Eq1 Solo where
  liftEq eq (Solo a) (Solo b) = a `eq` b

instance Ord1 Solo where
  liftCompare cmp (Solo a) (Solo b) = cmp a b

instance Read1 Solo where
#if MIN_VERSION_base(4,10,0)
    liftReadPrec rp _ = readData (readUnaryWith rp "Solo" Solo)

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault
#else
    liftReadsPrec rp _ = readsData $ readsUnaryWith rp "Solo" Solo
#endif

instance Show1 Solo where
    liftShowsPrec sp _ d (Solo x) = showParen (d > 10) $
      showString "Solo " . sp 11 x

#else
instance Eq1 Solo where eq1 = (==)
instance Ord1 Solo where compare1 = compare
instance Read1 Solo where readsPrec1 = readsPrec
instance Show1 Solo where showsPrec1 = showsPrec
#endif

#endif
