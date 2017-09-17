module Web.Reflex.Bootstrap.Tooltip(
    TooltipPlace(..)
  , tooltipPlace
  , initTooltip
  , hrefTooltip
  , btnTooltip
  ) where

import Control.Monad.IO.Class
import Data.Functor
import Data.Text (Text)
import GHC.Generics
import Reflex
import Reflex.Dom

import qualified JSDOM.Types as DOM
import Language.Javascript.JSaddle

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

-- foreign import javascript unsafe "$($1).tooltip();" js_initTooltip :: DOM.Element -> IO ()
js_initTooltip :: DOM.MonadJSM m => DOM.Element -> m ()
js_initTooltip e = liftJSM $ do
  f <- eval ("function(x) { $(x).tooltip(); }" :: String)
  this <- obj
  void $ call f this (toJSVal e)

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
initTooltip Element{..} = js_initTooltip _element_raw

-- | Create clickable link with subcontent and tooltip
hrefTooltip :: MonadWidget t m => TooltipPlace -> Text -> m a -> m (Event t ())
hrefTooltip place label ma = do
  (l,_) <- elAttr' "a" [
      ("href", "#")
    , ("onclick", "return false;")
    , ("data-toggle", "tooltip")
    , ("data-placement", tooltipPlace place)
    , ("title", "")
    , ("data-original-title", label)
    ] ma
  initTooltip l
  return $ domEvent Click l

-- | Create clickable link with subcontent and tooltip
btnTooltip :: MonadWidget t m => TooltipPlace -> Text -> m a -> m (Event t ())
btnTooltip place label ma = do
  (l,_) <- elAttr' "button" [
      ("type", "button")
    , ("class", "btn btn-default")
    , ("data-toggle", "tooltip")
    , ("data-placement", tooltipPlace place)
    , ("title", "")
    , ("data-original-title", label)
    ] ma
  initTooltip l
  return $ domEvent Click l
