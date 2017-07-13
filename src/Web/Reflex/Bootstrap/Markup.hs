module Web.Reflex.Bootstrap.Markup(
    ulClass
  , liClass
  , spanClass
  , li
  , header
  , handleDanger
  , danger
  , info
  , centered
  , strutWidgetY
  , container
  , panel
  , row
  , md2
  , md3
  , md4
  , md5
  , md6
  , md7
  , icon
  , well
  , href
  , primaryButton
  ) where

import Data.Monoid
import Data.Text (Text)
import Reflex
import Reflex.Dom

-- | Helper for ul element with class
ulClass :: MonadWidget t m => Text -> m a -> m a
ulClass = elClass  "ul"

-- | Helper for li element with class
liClass :: MonadWidget t m => Text -> m a -> m a
liClass = elClass  "li"

-- | Helper for span element with class
spanClass :: MonadWidget t m => Text -> m a -> m a
spanClass = elClass  "span"

-- | Helper for simple li element
li :: MonadWidget t m => m a -> m a
li = el "li"

-- | Helper to display centered header
header :: MonadWidget t m => Text -> m ()
header = elAttr "h1" [("style", "text-align: center;")] . text

-- | Display 'Left' occurences in danger well
handleDanger :: MonadWidget t m => Event t (Either Text a) -> m (Event t a)
handleDanger ea = do
  _ <- widgetHold (return ()) $ ffor ea $ \case
    Left e -> danger e
    Right _ -> return ()
  return $ fforMaybe ea $ \case
    Right a -> Just a
    _ -> Nothing

-- | Helper to dislpay text in red well
danger :: MonadWidget t m => Text -> m ()
danger = elClass "div" "alert alert-danger" . text

-- | Helper to dislpay text in blue well
info :: MonadWidget t m => Text -> m ()
info = elClass "div" "alert alert-info" . text

-- | Create wrapper div that is centered
centered :: MonadWidget t m => m a -> m a
centered w = elAttr "div" [("style", "text-align: center;")] $
  elAttr "div" [("style", "display: inline-block")] w

-- | Invisible div that fills vertical space
strutWidgetY :: MonadWidget t m => Text -- ^ Size parameter, ex "10px"
  -> m ()
strutWidgetY size = elAttr "div" [("style", "margin-top: " <> size <> ";")] $ return ()

container :: MonadWidget t m => m a -> m a
container = elClass "div" "container"

panel :: MonadWidget t m => m a -> m a
panel = elClass "div" "panel"

row :: MonadWidget t m => m a -> m a
row = elClass "div" "row"

md2 :: MonadWidget t m => m a -> m a
md2 = elClass "div" "col-md-2"

md3 :: MonadWidget t m => m a -> m a
md3 = elClass "div" "col-md-3"

md4 :: MonadWidget t m => m a -> m a
md4 = elClass "div" "col-md-4"

md5 :: MonadWidget t m => m a -> m a
md5 = elClass "div" "col-md-5"

md6 :: MonadWidget t m => m a -> m a
md6 = elClass "div" "col-md-6"

md7 :: MonadWidget t m => m a -> m a
md7 = elClass "div" "col-md-7"

-- | Embedd icon
icon :: MonadWidget t m => Text -> m ()
icon name = elClass "i" "material-icons" $ text name

-- | Bootstrap well panel
well :: MonadWidget t m => m a -> m a
well = elClass "div" "well"

-- | Create clickable link with subcontent
href :: MonadWidget t m => m a -> m (Event t ())
href ma = do
  (l,_) <- elAttr' "a" [("href", "#"), ("onclick", "return false;")] ma
  return $ domEvent Click l

-- | The most common bootstrap style for button
primaryButton :: MonadWidget t m => Text -> m (Event t ())
primaryButton s = do
  (e, _) <- elAttr' "a" [("class", "btn btn-raised btn-primary"), ("href", "javascript:void(0)")] $ text s
  return $ domEvent Click e
