-- | A variant data type, based on rows, but unlike
-- | `purescript-variant`, this one uses a representation
-- | which is compatible with Aeson, so that we can
-- | use the data we get from the server directly.
module Data.TaggedSum
  ( module Reexports
  ) where

import Data.TaggedSum.Internal
  ( TaggedSum
  , getTag
  , tag
  , tag_
  , cotag
  , cotag_
  , case_
  , match
  ) as Reexports
