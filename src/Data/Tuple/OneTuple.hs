{-# LANGUAGE PatternSynonyms #-}
-- | This is a module to help migration from @OneTuple@ to @Solo@.
-- Migrate to use "Data.Tuple" from @base-4.16@ or "Data.Tuple.Solo" with all GHCs.
--
-- The pattern synonym is provided for GHCs supporting pattern synonyms (7.8+)
module Data.Tuple.OneTuple
{-# DEPRECATED "Use Data.Tuple.Solo" #-}
(
    OneTuple,
    pattern OneTuple,
    only,
) where

import Data.Tuple.Solo

type OneTuple = Solo

only :: OneTuple a -> a
only = getSolo

pattern OneTuple :: a -> Solo a
pattern OneTuple a = MkSolo a

{-# COMPLETE OneTuple #-}
