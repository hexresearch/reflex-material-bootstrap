module Web.Reflex.Bootstrap.Menu(
    menuWidget
  , MenuWidget(..)
  ) where

import Control.Monad
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Proxy
import Data.Text
import Reflex.Dom

import qualified Data.Map.Strict as M

import Web.Reflex.Bootstrap.Markup
import Web.Reflex.Bootstrap.Router

-- | Describe your menu items, how to render them
class (Ord (MenuItem a), Eq (MenuItem a)) => MenuWidget a where
  -- | Your type to describe menu item (usally a Enum)
  type MenuItem a :: *

  -- | Which test to display on menu brand label
  menuBrand :: Proxy a -> Text

  -- | Render menu item label on button
  menuItemLabel :: Proxy a -> MenuItem a -> Text

  -- | Which label to use for logout button
  menuLogoutLabel :: Proxy a -> Text

-- | Create menu widget that switches over pages of the app
menuWidget :: forall t m a . (MonadWidget t m, MenuWidget a)
  => Proxy a
  -> MenuItem a -- ^ Name of startup page
  -> Map (MenuItem a) (m (Event t (MenuItem a))) -- ^ Mappings from page name to widget, return event allows to jump to another page
  -> m (Event t ()) -- ^ Event when user wants to logout
menuWidget prox initialItem items = fmap (switch . current) . route $ mdo
  r <- menuBar ne initialItem
  ne <- case M.lookup initialItem items of
    Nothing -> pure never
    Just m -> m
  pure r
  where
  menuBar :: Event t (MenuItem a) -> MenuItem a -> m (Event t (), Route t m (Event t ()))
  menuBar nextE currItem = divClass "navbar navbar-default" $ divClass "container-fluid" $ do
    divClass "navbar-header" $ do
      elAttr "button" [
          ("type", "button")
        , ("class", "navbar-toggle")
        , ("data-toggle", "collapse")
        , ("data-target", ".navbar-inverse-control")
        ] $ do
        replicateM_ 3 $ spanClass "icon-bar" $ return ()
      elAttr "a" [
          ("class", "navbar-brand")
        , ("href", "javascript:void(0)")
        ] $ text $ menuBrand prox
    divClass "navbar-collapse collapse navbar-inverse-collapse" $ do
      ulClass "nav navbar-nav" $ mdo
        routes <- forM (M.toList items) $ \(name, m) -> if name == currItem
          then liClass "active" $ do
            _ <- link $ menuItemLabel prox name
            pure $ Route never
          else li $ do
            e <- href . text $ menuItemLabel prox name
            pure $ Route . ffor e $ const $ mdo
              r <- menuBar e name
              e <- m
              pure r
        logoutE <- li . href . text $ menuLogoutLabel prox
        -- Route for event when current widget want to change page
        let manualRoute = Route . fforMaybe nextE $ \name -> case M.lookup name items of
              Nothing -> Nothing
              Just m -> Just $ mdo
                r <- menuBar e name
                e <- m
                pure r
        pure (logoutE, manualRoute <> mconcat routes)
