{-|
Module      : Web.Reflex.Bootstrap.Form
Description : Rudimentary support for bootstrap forms
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable

Example of usage:
@
authForm :: MonadWidget t m => Dynamic t (Login, Password)
authForm = horizontalForm $ do
  loginInput <- formGroupText "Login" def
  passInput <- formGroupText "Password" def { _textInputConfig_inputType = "password" }
  return $ (,) <$> loginInput <*> passInput
@
-}
module Web.Reflex.Bootstrap.Form(
    horizontalForm
  , formGroupStatic
  , formGroupText
  , formGroupInt
  , formGroupJson
  , formGroupLabel
  , formGroupSelect
  , textInputDyn
  , editInputDyn
  , submitButton
  ) where

import Data.Aeson
import Data.Bifunctor
import Data.Default
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text
import Reflex
import Reflex.Dom
import Text.Read

import Web.Reflex.Bootstrap.Markup
import Web.Reflex.Bootstrap.Upload.Input
import Web.Reflex.Bootstrap.Utils

-- | Wrapper for bootstrap horizontal form
horizontalForm :: MonadWidget t m => m a -> m a
horizontalForm = elAttr "form" [
    ("class", "form-horizontal")
  , ("accept-charset", "UTF-8")
  ]

-- | Helper to create bootstrap text input with label
formGroupJson :: forall t m a . (FromJSON a, MonadWidget t m)
  => Dynamic t Text -- ^ Label
  -> UploadFileConfig t -- ^ Input field config
  -> m (Event t (Either Text (FullUploadFile a)))
formGroupJson labelTextD cfg = formGroup $ do
  initialLabel <- sample . current $ labelTextD
  mkLabel [ ("for", elemId initialLabel)
          , ("class", "col-md-2 control-label")] $ dynText labelTextD
  elClass "div" "col-md-12" $ do
    rec
      _ <- textInput def {
          _textInputConfig_attributes = ffor fileNameD $ \name -> [
              ("class", "form-control")
            , ("placeholder", name)
            , ("readonly", "")]
        }
      mfileE <- uploadJsonFileInput cfg {
          uploadFileInputAttrs = (mappend [
              ("class", "form-control")
            , ("id", elemId initialLabel)]
            ) <$> uploadFileInputAttrs cfg
        }
      let fileNameE = fforMaybe mfileE $ \case
            Right FullUploadFile{..} -> Just uploadFullFileName
            _ -> Nothing
      fileNameD <- holdDyn "Browse..." fileNameE
    return mfileE
  where
    formGroup = elClass "div" "form-group"
    mkLabel = elAttr "label"
    elemId = ("input" <>)

-- | Wrap with a form group label
formGroupLabel :: MonadWidget t m => Dynamic t Text -> m a -> m a
formGroupLabel labelTextD ma = formGroup $ do
  mkLabel [ ("class", "col-md-2 control-label")] $ dynText labelTextD
  elClass "div" "col-md-12" ma
  where
    formGroup = elClass "div" "form-group"
    mkLabel = elAttr "label"

-- | Helper to create bootstrap text input with label
formGroupText :: MonadWidget t m => Dynamic t Text -> TextInputConfig t -> m (TextInput t)
formGroupText labelTextD cfg = formGroup $ do
  initialLabel <- sample . current $ labelTextD
  mkLabel [ ("for", elemId initialLabel)
          , ("class", "col-md-2 control-label")] $ dynText labelTextD
  elClass "div" "col-md-12" $ textInput cfg {
      _textInputConfig_attributes = constDyn [
          ("class", "form-control")
        , ("id", elemId initialLabel)
        , ("type", "text")
        ]
    }
  where
    formGroup = elClass "div" "form-group"
    mkLabel = elAttr "label"
    elemId = ("input" <>)

-- | Helper to create bootstrap static text field with label
formGroupStatic :: MonadWidget t m => Dynamic t Text -> Dynamic t Text -> m (TextInput t)
formGroupStatic labelTextD valD = formGroup $ do
  initialLabel <- sample . current $ labelTextD
  mkLabel [ ("for", elemId initialLabel)
          , ("class", "col-md-2 control-label")] $ dynText labelTextD
  v <- sample $ current valD
  elClass "div" "col-md-12" $ textInput TextInputConfig {
      _textInputConfig_setValue = updated valD
    , _textInputConfig_initialValue = v
    , _textInputConfig_inputType = "text"
    , _textInputConfig_attributes = constDyn [
          ("class", "form-control")
        , ("id", elemId initialLabel)
        , ("type", "text")
        , ("readonly", "")
        ]
    }
  where
    formGroup = elClass "div" "form-group"
    mkLabel = elAttr "label"
    elemId = ("input" <>)

-- | Helper to create bootstrap select input with label.
--
-- Create a dropdown box The first argument gives the initial value of the dropdown; if it is not present in the map of options provided, it will be added with an empty string as its text
formGroupSelect :: (MonadWidget t m, Ord k, Show k, Read k) => Dynamic t Text -> k -> Dynamic t (Map k Text) -> DropdownConfig t k -> m (Dropdown t k)
formGroupSelect labelTextD initKey vals cfg = formGroup $ do
  initialLabel <- sample . current $ labelTextD
  mkLabel [ ("for", elemId initialLabel)
          , ("class", "col-md-2 control-label")] $ dynText labelTextD
  elClass "div" "col-md-12" $ dropdown initKey vals cfg {
      _dropdownConfig_attributes = do
        atrs <- _dropdownConfig_attributes cfg
        pure $ atrs <> [
            ("class", "form-control")
          , ("id", elemId initialLabel)
          ]
    }
  where
    formGroup = elClass "div" "form-group"
    mkLabel = elAttr "label"
    elemId = ("input" <>)

-- | Configuration for 'formGroupInt' widget
data IntInputConfig t = IntInputConfig {
  intInputInitVal :: Int
, intInputSetVal  :: Event t Int
, intInputAttrs   :: Dynamic t (Map Text Text)
}

instance Reflex t => Default (IntInputConfig t) where
  def = IntInputConfig {
      intInputInitVal = 0
    , intInputSetVal = never
    , intInputAttrs = pure mempty
    }

-- | Helper to create bootsrap integer input with label
formGroupInt :: MonadWidget t m => Dynamic t Text -> IntInputConfig t -> m (Dynamic t Int)
formGroupInt labelTextD IntInputConfig{..} = formGroup $ do
  initialLabel <- sample . current $ labelTextD
  mkLabel [ ("for", elemId initialLabel)
          , ("class", "col-md-2 control-label")] $ dynText labelTextD
  tinput <- elClass "div" "col-md-12" $ textInput TextInputConfig {
      _textInputConfig_inputType    = "number"
    , _textInputConfig_initialValue = showt intInputInitVal
    , _textInputConfig_setValue     = showt <$> intInputSetVal
    , _textInputConfig_attributes   = mappend [
          ("class", "form-control")
        , ("id", elemId initialLabel)
        , ("type", "number")] <$> intInputAttrs
    }
  holdDyn intInputInitVal $ fforMaybe (updated $ value tinput) $ readMaybe . unpack
  where
    formGroup = elClass "div" "form-group"
    mkLabel = elAttr "label"
    elemId = ("input" <>)

-- | Field that transforms dynamic without label
textInputDyn :: MonadWidget t m => Dynamic t Text -> m (Dynamic t Text)
textInputDyn dv = do
  dv0 <- sample . current $ dv
  ti <- textInput TextInputConfig {
      _textInputConfig_inputType = "text"
    , _textInputConfig_initialValue = dv0
    , _textInputConfig_setValue = updated dv
    , _textInputConfig_attributes = pure [("class", "form-control")]
    }
  pure $ _textInput_value ti

-- | Field to edit values that are showable and readable from string
editInputDyn :: (Read a, Show a, MonadWidget t m) => Dynamic t a -> m (Dynamic t a)
editInputDyn dv = do
  dv0 <- sample . current $ dv
  tinput <- textInput TextInputConfig {
      _textInputConfig_inputType    = "text"
    , _textInputConfig_initialValue = showt dv0
    , _textInputConfig_setValue     = showt <$> updated dv
    , _textInputConfig_attributes   = pure [("class", "form-control")]
    }
  let newValE = fmap (first pack . readEither . unpack) . updated . value $ tinput
  filteredValE <- handleDanger newValE
  holdDyn dv0 filteredValE

-- | Helper to make form submit button
submitButton :: MonadWidget t m => Dynamic t Text -> m (Event t ())
submitButton sd = do
  (e, _) <- elAttr' "button" [("type", "button"), ("class", "btn btn-primary")] $ dynText sd
  return $ domEvent Click e
