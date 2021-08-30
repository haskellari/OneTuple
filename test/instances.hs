module Main where

import Data.Ix (Ix)
import Data.Monoid         (Monoid (..))
import Data.Semigroup      (Semigroup (..))
import Control.Applicative (Applicative (..))
import Control.Monad.Fix   (MonadFix (..))
import Data.Traversable    (Traversable (..))
import Data.Foldable       (Foldable (..))

import Data.Tuple.OneTuple (OneTuple (..))

main :: IO ()
main = putStrLn "works"

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

tup1 :: OneTuple Char
tup1 = OneTuple 'x'

tup2 :: OneTuple String
tup2 = OneTuple "test"

hasEq :: Eq a => a -> a; hasEq x = x; testEq = hasEq tup1
hasOrd :: Ord a => a -> a; hasOrd x = x; testOrd = hasOrd tup1
hasBounded :: Bounded a => a -> a; hasBounded x = x; testBounded = hasBounded tup1
hasEnum :: Enum a => a -> a; hasEnum x = x; testEnum = hasEnum tup1
hasShow :: Show a => a -> a; hasShow x = x; testShow = hasShow tup1
hasRead :: Read a => a -> a; hasRead x = x; testRead = hasRead tup1
hasIx :: Ix a => a -> a; hasIx x = x; testIx = hasIx tup1
hasMonoid :: Monoid a => a -> a; hasMonoid x = x; testMonoid = hasMonoid tup2
hasSemigroup :: Semigroup a => a -> a; hasSemigroup x = x; testSemigroup = hasSemigroup tup2

hasFunctor :: Functor f => f a -> f a; hasFunctor x = x; testFunctor = hasFunctor tup1
hasFoldable :: Foldable f => f a -> f a; hasFoldable x = x; testFoldable = hasFoldable tup1
hasTraversable :: Traversable f => f a -> f a; hasTraversable x = x; testTraversable = hasTraversable tup1
hasApplicative :: Applicative f => f a -> f a; hasApplicative x = x; testApplicative = hasApplicative tup1
hasMonad :: Monad f => f a -> f a; hasMonad x = x; testMonad = hasMonad tup1
hasMonadFix :: MonadFix f => f a -> f a; hasMonadFix x = x; testMonadFix = hasMonadFix tup1
