module Web.Reflex.Bootstrap.Tooltip(
    TooltipPlace(..)
  , tooltipPlace
  , initTooltip
  , hrefTooltip
  , simpleTooltip
  , btnTooltip
  ) where

import Control.Monad.IO.Class
import Data.Text (Text)
import GHC.Generics
import Reflex
import Reflex.Dom

import qualified GHCJS.DOM.Types as DOM

-- | Placement of tooltip relative to element
data TooltipPlace = TooltipLeft | TooltipRight | TooltipBottom | TooltipTop
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | Transform into HTML attribute value
tooltipPlace :: TooltipPlace -> Text
tooltipPlace p = case p of
  TooltipLeft -> "left"
  TooltipRight -> "right"
  TooltipBottom -> "bottom"
  TooltipTop -> "top"

foreign import javascript unsafe "$($1).tooltip();" js_initTooltip :: DOM.Element -> IO ()

-- | Initialize tooltip for element (raw element from 'el'' and 'elClass'')
--
-- Don't forget to add this to the end of HTML file:
-- @
-- <script>
--   $(document).ready(function(){
--       $.material.init();
--   });
-- </script>
--
initTooltip :: MonadWidget t m => Element EventResult (DomBuilderSpace m) t -> m ()
initTooltip Element{..} = liftIO $ js_initTooltip _element_raw

-- | Create clickable link with subcontent and tooltip
hrefTooltip :: MonadWidget t m => TooltipPlace -> Dynamic t Text -> m a -> m (Event t ())
hrefTooltip place labelD ma = do
  (l,_) <- elDynAttr' "a" (do
    label <- labelD
    pure [
        ("href", "#")
      , ("onclick", "return false;")
      , ("data-toggle", "tooltip")
      , ("data-placement", tooltipPlace place)
      , ("title", "")
      , ("data-original-title", label)
      ]) ma
  initTooltip l
  return $ domEvent Click l

-- | Create wrapper that raises tooltiop
simpleTooltip :: MonadWidget t m => TooltipPlace -> Dynamic t Text -> m a -> m a
simpleTooltip place labelD ma = do
  (l, a) <- elDynAttr' "div" (do
    label <- labelD
    pure [
        ("data-toggle", "tooltip")
      , ("data-placement", tooltipPlace place)
      , ("title", "")
      , ("data-original-title", label)
      ]) ma
  initTooltip l
  pure a

-- | Create clickable link with subcontent and tooltip
btnTooltip :: MonadWidget t m => TooltipPlace -> Dynamic t Text -> m a -> m (Event t ())
btnTooltip place labelD ma = do
  (l,_) <- elDynAttr' "button" (do
    label <- labelD
    pure [
        ("type", "button")
      , ("class", "btn btn-default")
      , ("data-toggle", "tooltip")
      , ("data-placement", tooltipPlace place)
      , ("title", "")
      , ("data-original-title", label)
      ]) ma
  initTooltip l
  return $ domEvent Click l
