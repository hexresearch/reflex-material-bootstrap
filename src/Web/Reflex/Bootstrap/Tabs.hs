module Web.Reflex.Bootstrap.Tabs(
    pills
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Data.Text (Text)
import Reflex.Dom

import qualified Data.Map.Strict as M

import Web.Reflex.Bootstrap.Markup
import Web.Reflex.Bootstrap.Router

-- | Make widget with tabs from several widgets for pages
pills :: forall t m a . MonadWidget t m
  => Text -- ^ Startup widget name (to highlight in menu)
  -> m a -- ^ Startup widget
  -> Map Text (m a) -- ^ Mappings from page name to widget
  -> m (Dynamic t a)
pills initialName initialItem items = route $ do
  r <- pillsBar initialName
  a <- initialItem
  pure (a, r)
  where
  pillsBar :: Text -> m (Route t m a)
  pillsBar currItem = elClass "ul" "nav nav-pills" $ do
    routes <- forM (M.toList items) $ \(name, m) -> if name == currItem
      then liClass "active" $ do
        _ <- link name
        pure $ Route never
      else li $ do
        e <- href $ text name
        pure $ Route . ffor e $ const $ do
          r <- pillsBar name
          a <- m
          pure (a, r)
    pure $ mconcat routes

-- <ul class="nav nav-pills">
--   <li class="active"><a href="javascript:void(0)">Home</a></li>
--   <li><a href="javascript:void(0)">Profile</a></li>
--   <li class="disabled"><a href="javascript:void(0)">Disabled</a></li>
--   <li class="dropdown">
--     <a class="dropdown-toggle" data-toggle="dropdown" href="bootstrap-elements.html" data-target="#">
--       Dropdown <span class="caret"></span>
--     </a>
--     <ul class="dropdown-menu">
--       <li><a href="javascript:void(0)">Action</a></li>
--       <li><a href="javascript:void(0)">Another action</a></li>
--       <li><a href="javascript:void(0)">Something else here</a></li>
--       <li class="divider"></li>
--       <li><a href="javascript:void(0)">Separated link</a></li>
--     </ul>
--   </li>
-- </ul>
