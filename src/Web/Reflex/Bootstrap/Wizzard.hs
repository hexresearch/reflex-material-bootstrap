{-# LANGUAGE TemplateHaskell #-}
module Web.Reflex.Bootstrap.Wizzard(
    WizzardGraph(..)
  , WizzardStep(..)
  , wizzardStepName
  , wizzardStepBody
  , WizzardConfig(..)
  , wizzardConfigNextLabel
  , wizzardConfigBackLabel
  , wizzard
  ) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Reflex.Dom

import Web.Reflex.Bootstrap.Markup
import Web.Reflex.Bootstrap.Router
import Web.Reflex.Bootstrap.Utils

-- | Allowed ways to compose wizzard steps. Allows only list-like graph for
-- wizzard.
data WizzardGraph t m a b where
  -- | Last (or the least) step of wizzard step
  WizzardSingle :: (a -> WizzardStep t m b) -> WizzardGraph t m a b
  -- | Chain steps in wizzard
  WizzardChain :: (a -> WizzardStep t m c) -> WizzardGraph t m c b -> WizzardGraph t m a b

-- | Descrption of widget step
data WizzardStep t m a = WizzardStep {
  _wizzardStepName :: Text -- ^ Name of the step displayed in pill body
  -- | Current value of result, when the value is Just, next button is enabled
  -- and event for immediate change to next step. Input argument is last seen
  -- state of the body when user wants to go back.
, _wizzardStepBody :: Maybe a -> m (Dynamic t (Maybe a), Event t a)
}
makeLenses ''WizzardStep

-- | Global configuration for wizzard widget
data WizzardConfig = WizzardConfig {
  _wizzardConfigNextLabel :: Text -- ^ Label for Next button
, _wizzardConfigBackLabel :: Text -- ^ Label for Prev button
}
makeLenses ''WizzardConfig

instance Default WizzardConfig where
  def = WizzardConfig {
      _wizzardConfigNextLabel = "Next"
    , _wizzardConfigBackLabel = "Back"
    }

-- | Create wizzard from given wizzard step DSL, fire event on last screen with
-- result of last widget.
wizzard :: forall t m a . MonadWidget t m => WizzardConfig -> WizzardGraph t m () a -> m (Event t a)
wizzard WizzardConfig{..} = fmap switchPromptlyDyn . route . wizzard' [] ()
  where
    wizzard' :: forall b c . [(Text, m (Event t c, Route t m (Event t c)))] -> b -> WizzardGraph t m b c -> m (Event t c, Route t m (Event t c))
    wizzard' steps curV (WizzardSingle mkStep) = do
      let step = mkStep curV
      pillsRoute <- wizzardHeader steps (_wizzardStepName step)
      (curValD, manualNextE) <- el "div" $ _wizzardStepBody step Nothing
      backRoute <- case steps of
        [] -> pure . Route $ never
        _ -> do
          let (_, mr) = last steps
          e <- primaryButton _wizzardConfigBackLabel
          pure . Route $ const mr <$> e
      nextE <- primaryRightButtonDisable _wizzardConfigNextLabel $ isJust <$> curValD
      let nextValE = leftmost [
              fmapMaybe id (current curValD `tag` nextE)
            , manualNextE
            ]
      pure (nextValE, pillsRoute <> backRoute)

    wizzard' steps curV curGraph@(WizzardChain mkStep graph) = do
      let step = mkStep curV
          curName = _wizzardStepName step
      pillsRoute <- wizzardHeader steps curName
      (curValD, manualNextE) <- el "div" $ _wizzardStepBody step Nothing
      backRoute <- case steps of
        [] -> pure . Route $ never
        _ -> do
          let (_, mr) = last steps
          e <- primaryButton _wizzardConfigBackLabel
          pure . Route $ const mr <$> e
      nextE <- primaryRightButtonDisable _wizzardConfigNextLabel $ isJust <$> curValD
      let nextValE = leftmost [
              fmapMaybe id (current curValD `tag` nextE)
            , manualNextE
            ]
          myPill v = (curName, wizzard' steps curV curGraph)
          nextRoute = Route $ (\v -> wizzard' (steps ++ [myPill v]) v graph) <$> nextValE
      pure (never, nextRoute <> pillsRoute <> backRoute)

    wizzardHeader :: [(Text, m (c, Route t m c))] -> Text -> m (Route t m c)
    wizzardHeader is curItem = elClass "ul" "nav nav-pills" $ do
      allr <- forM is $ \(name, mr) -> do
        e <- li . href . text $ name
        pure . Route $ const mr <$> e
      _ <- liClass "active" . href . text $ curItem
      pure $ mconcat allr

-- | Primary button with disable flag
primaryRightButtonDisable :: MonadWidget t m => Text -> Dynamic t Bool -> m (Event t ())
primaryRightButtonDisable s enableD = fmap switchPromptlyDyn . widgetHoldDyn $ ffor enableD $ \v -> if v
  then do
    (e, _) <- elAttr' "a" [("class", "btn btn-raised pull-right btn-primary"), ("href", "javascript:void(0)")] $ text s
    return $ domEvent Click e
  else do
    elAttr "fieldset" [("disabled", "")] $ do
      elAttr "a" [("class", "btn btn-raised pull-right btn-primary"), ("href", "javascript:void(0)")] $ text s
      pure never
