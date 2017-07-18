{-# LANGUAGE TypeFamilies #-}
module Web.Reflex.Bootstrap.Basket(
    BasketItem(..)
  , basketList
  ) where

import Data.Map.Strict (Map)
import Data.Monoid
import Data.Proxy
import Reflex.Dom

import qualified Data.Map.Strict as M

-- | Additonal behavior needed for 'basketList' widget
class (Eq (BasketItemId a), Ord (BasketItemId a)) => BasketItem a where
  -- | Item id that allows to distinguish items from each other
  type BasketItemId a :: *

  -- | Get item id
  basketElementId :: a -> BasketItemId a

  -- | Render basket row and return event when user clicked on deletion widget
  -- (user of the widget should provide it himself).
  --
  -- The dynamic argument changes when an user adds again the same item.
  renderBasketItem :: MonadWidget t m => Dynamic t a -> m (Event t ())

  -- | Wrapper around basket body (for instance, table header)
  renderBasketHeader :: MonadWidget t m => Proxy a -> m b -> m b

-- | Display collection of items and allow to delete them for user. Doesn't provide
-- a pagination.
basketList :: forall t m a . (MonadWidget t m, BasketItem a)
  => [a] -- ^ Initial set of items
  -> Event t a -- ^ Addition of new items
  -> m (Dynamic t [a])
basketList initItems addE = renderBasketHeader (Proxy :: Proxy a) $ mdo
  let addMapE, changeMapE :: Event t (Map (BasketItemId a) (Maybe a))
      addMapE = ffor addE $ \v -> M.singleton (basketElementId v) (Just v)
      changeMapE = addMapE <> delMapE
      initMap = M.fromList $ fmap basketElementId initItems `zip` initItems
  mapD :: Dynamic t (Map (BasketItemId a) (a, Event t ())) <- listWithKeyShallowDiff initMap changeMapE $ \_ v ev -> do
    delE <- renderBasketItem =<< holdDyn v ev
    pure (v, delE)
  let delMapE :: Event t (Map (BasketItemId a) (Maybe a))
      delMapE = switchPromptlyDyn $ fmap (fmap $ const Nothing) . mergeMap . fmap snd <$> mapD
  pure $ fmap fst . M.elems <$> mapD
