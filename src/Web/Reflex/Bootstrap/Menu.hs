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
import Web.Reflex.Bootstrap.Utils

-- | Describe your menu items, how to render them
class (MonadWidget t m, Ord (MenuItem a), Eq (MenuItem a)) => MenuWidget t m a | a -> m, m -> t where
  -- | Your type to describe menu item (usally a Enum)
  type MenuItem a :: *

  -- | Which test to display on menu brand label
  menuBrand :: Proxy a -> m (Dynamic t Text)

  -- | Render menu item label on button
  menuItemLabel :: Proxy a -> MenuItem a -> m (Dynamic t Text)

  -- | Which label to use for logout button
  menuLogoutLabel :: Proxy a -> m (Dynamic t Text)

  -- | Additional classes that should be added to the generated navbar
  menuBarClasses :: Proxy a -> m (Dynamic t Text)
  menuBarClasses _ = pure $ pure "navbar-expand-lg navbar-dark bg-dark justify-content-between"

  -- | Render additional widget on the right side next to logout
  menuAdditionalRightWidget :: Proxy a -> m ()
  menuAdditionalRightWidget _ = pure ()

-- | Create menu widget that switches over pages of the app
menuWidget :: forall t m a . MenuWidget t m a
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
  menuBar nextE currItem = do
    classesD <- fmap ("navbar " <>) <$> menuBarClasses prox
    elDynAttr "div" ((\classes -> [("class", classes)]) <$> classesD) $ do
      -- Brand
      brandTextD <- menuBrand prox
      dyn $ ffor brandTextD $ \brandText -> elAttr "a" [
          ("class", "navbar-brand")
        , ("href", "#")
        ] $ text brandText
      -- Mobile menu toggler
      elAttr "button" [
          ("class", "navbar-toggler")
        , ("type", "button")
        , ("data-toggle", "collapse")
        , ("data-target", "#navbarSupportedContent")
        , ("aria-controls", "navbarSupportedContent")
        , ("aria-expanded", "false")
        , ("aria-label", "Toggle navigation")
        ] $ spanClass "navbar-toggler-icon" $ pure ()
      -- Content
      elAttr "div" [("class", "collapse navbar-collapse"), ("id", "navbarSupportedContent")] $ mdo
        routes <- ulClass "navbar-nav mr-auto" $ mdo
          forM (M.toList items) $ \(name, m) -> if name == currItem
            then liClass "nav-item active" $ do
              itemLabelD <- menuItemLabel prox name
              _ <- dyn $ ffor itemLabelD $ flip linkClass "nav-link"
              pure $ Route never
            else liClass "nav-item" $ do
              itemLabelD <- menuItemLabel prox name
              e <- widgetHoldDyn $ ffor itemLabelD $ hrefClass "nav-link" . text
              pure $ Route . ffor (switchPromptlyDyn e) $ const $ mdo
                r <- menuBar e name
                e <- m
                pure r
        menuAdditionalRightWidget prox
        logoutLabelD <- menuLogoutLabel prox
        logoutE <- widgetHoldDyn $ ffor logoutLabelD $ hrefClass "nav-link" . text
        -- Route for event when current widget want to change page
        let manualRoute = Route . fforMaybe nextE $ \name -> case M.lookup name items of
              Nothing -> Nothing
              Just m -> Just $ mdo
                r <- menuBar e name
                e <- m
                pure r
        pure (switchPromptlyDyn logoutE, manualRoute <> mconcat routes)
