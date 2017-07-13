{-|
Module      : Web.Reflex.Bootstrap.Table
Description : Table helpers for bootstrap
Copyright   : (c) Anton Gushcha, 2017
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable

-}
module Web.Reflex.Bootstrap.Table(
    tableHover
  , thead
  , tr
  , trClass
  , th
  , tbody
  , td
  ) where

import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text)
import Reflex.Dom

import qualified Data.Map.Strict as M

-- | Same as 'adjust' but inserts default value if cannot find key
adjert :: Ord k => v -> (v -> v) -> k -> Map k v -> Map k v
adjert v0 f k m = case M.lookup k m of
  Nothing -> M.insert k v0 m
  Just v -> M.insert k (f v) m

-- | Wraps contents into table
tableHover :: forall t m a . MonadWidget t m => Map Text Text -> m a -> m a
tableHover attrs ma = elAttr "table" attrs' ma
  where
    classes = "table table-striped table-hover "
    attrs' = adjert classes (classes <>) "class" attrs

-- | Wraps contents in <thead> tag
thead :: forall t m a . MonadWidget t m => m a -> m a
thead = el "thead"

-- | Wrapps contents in <tr> tag
tr :: forall t m a . MonadWidget t m => m a -> m a
tr = el "tr"

-- | Wrapps contents in <tr> tag
trClass :: forall t m a . MonadWidget t m => Text -> m a -> m a
trClass = elClass "tr"

-- | Wrapps contents in <th> tag
th :: forall t m a . MonadWidget t m => m a -> m a
th = el "th"

-- | Wrapps contents in <tbody> tag
tbody :: forall t m a . MonadWidget t m => m a -> m a
tbody = el "tbody"

-- | Wrapps contents in <td> tag
td :: forall t m a . MonadWidget t m => m a -> m a
td = el "td"
