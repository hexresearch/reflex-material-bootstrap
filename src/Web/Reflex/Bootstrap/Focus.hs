-- | Focus manipulation utils
module Web.Reflex.Bootstrap.Focus(
    FocusElement(..)
  ) where

import Control.Monad.IO.Class
import Data.Functor
import Data.Text (Text)
import Language.Javascript.JSaddle
import qualified JSDOM.Types as DOM
import Reflex.Dom

-- foreign import javascript unsafe "$1.focus()" js_focus :: DOM.Element -> IO ()
js_focus :: MonadJSM m => DOM.Element -> m ()
js_focus e = liftJSM $ do
  f <- eval ("function(x) {x.focus()}" :: Text)
  this <- obj
  void $ call f this (toJSVal e)

-- foreign import javascript unsafe "$1.scrollIntoView()" js_scrollIntoView :: DOM.Element -> IO ()
js_scrollIntoView :: MonadJSM m => DOM.Element -> m ()
js_scrollIntoView e = liftJSM $ do
  f <- eval ("function(x) {x.scrollIntoView()}" :: Text)
  this <- obj
  void $ call f this (toJSVal e)

-- | Focus element given in event payload
focusElementRaw :: forall t m . MonadWidget t m => Event t DOM.Element -> m ()
focusElementRaw e = performEvent_ . ffor e $ \v -> do
  js_focus v
  js_scrollIntoView v

-- | Scroll element given in event payload
scrollElementRaw :: forall t m . MonadWidget t m => Event t DOM.Element -> m ()
scrollElementRaw e = performEvent_ . ffor e $ js_scrollIntoView

-- | Overloaded focus setter
class FocusElement a where
  focusElement :: forall t m . MonadWidget t m => Event t a -> m ()
  scrollElement :: forall t m . MonadWidget t m => Event t a -> m ()

instance FocusElement DOM.Element where
  focusElement = focusElementRaw
  scrollElement = scrollElementRaw
  {-# INLINE focusElement #-}
  {-# INLINE scrollElement #-}

instance FocusElement DOM.HTMLInputElement where
  focusElement = focusElementRaw . fmap DOM.toElement
  scrollElement = scrollElementRaw . fmap DOM.toElement
  {-# INLINE focusElement #-}
  {-# INLINE scrollElement #-}

instance FocusElement (Element EventResult GhcjsDomSpace t) where
  focusElement = focusElementRaw . fmap _element_raw
  scrollElement = scrollElementRaw . fmap _element_raw
  {-# INLINE focusElement #-}
  {-# INLINE scrollElement #-}

instance FocusElement (InputElement EventResult GhcjsDomSpace t) where
  focusElement = focusElement . fmap _inputElement_element
  scrollElement = scrollElement . fmap _inputElement_element
  {-# INLINE focusElement #-}
  {-# INLINE scrollElement #-}

instance FocusElement (TextInput t) where
  focusElement = focusElement . fmap _textInput_element
  scrollElement = scrollElement . fmap _textInput_element
  {-# INLINE focusElement #-}
  {-# INLINE scrollElement #-}

instance FocusElement (RangeInput t) where
  focusElement = focusElement . fmap _rangeInput_element
  scrollElement = scrollElement . fmap _rangeInput_element
  {-# INLINE focusElement #-}
  {-# INLINE scrollElement #-}

instance FocusElement (TextArea t) where
  focusElement = focusElement . fmap (DOM.toElement . _textArea_element)
  scrollElement = scrollElement . fmap (DOM.toElement . _textArea_element)
  {-# INLINE focusElement #-}
  {-# INLINE scrollElement #-}

instance FocusElement (FileInput GhcjsDomSpace t) where
  focusElement = focusElement . fmap _fileInput_element
  scrollElement = scrollElement . fmap _fileInput_element
  {-# INLINE focusElement #-}
  {-# INLINE scrollElement #-}
