
-- |OneTuple fills the /tuple gap/ with a singleton tuple.
-- 
-- OneTuple /does not support/ the usual parenthesized tuple syntax.
-- 
-- OneTuple
--
--   * has the expected laziness properties
--
--   * can be pattern-matched
--
--   * ships with instances for several standard type classes,
--     including all those supported by H98-standard tuples
--
--   * requires no language extensions, except for hierarchical modules

module Data.Tuple.OneTuple (OneTuple(OneTuple), only) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Foldable
import Data.Ix
import Data.Monoid
import Data.Traversable

-- |OneTuple is the singleton tuple data type.
data OneTuple a
    = OneTuple a  -- ^ singleton tuple constructor
    deriving (Eq,Ord,Bounded,Show,Read)

-- |The 'only' function extracts the OneTuple's only member.
-- (Compare to 'fst' and 'snd'.)
only :: OneTuple a -- ^ takes a singleton tuple argument
     -> a          -- ^ returns the only element in the tuple
only (OneTuple x) = x

instance (Enum a) => Enum (OneTuple a) where
    succ = liftA succ
    pred = liftA pred
    toEnum = pure . toEnum
    fromEnum (OneTuple x) = fromEnum x

instance (Ix a) => Ix (OneTuple a) where
    range   (OneTuple x, OneTuple y) = map OneTuple (range (x,y))
    index   (OneTuple x, OneTuple y) (OneTuple z) = index   (x,y) z
    inRange (OneTuple x, OneTuple y) (OneTuple z) = inRange (x,y) z

instance Foldable OneTuple where
    fold (OneTuple m) = m
    foldMap f (OneTuple x) = f x
    foldr f b (OneTuple x) = f x b
    foldl f a (OneTuple x) = f a x
    foldr1 _f (OneTuple x) = x
    foldl1 _f (OneTuple x) = x

instance Traversable OneTuple where
    sequenceA (OneTuple x) = fmap OneTuple x

instance Functor OneTuple where
    fmap f (OneTuple x) = OneTuple (f x)

instance Applicative OneTuple where
    pure = return
    (<*>) = ap

instance Monad OneTuple where
    (OneTuple x) >>= f = f x
    return = OneTuple

instance (Monoid a) => Monoid (OneTuple a) where
    mempty = OneTuple mempty
    mappend (OneTuple x) (OneTuple y) = OneTuple (mappend x y)
    mconcat = Prelude.foldr1 mappend

instance MonadFix OneTuple where
    mfix f = let a = f (only a) in a

