{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Web.Reflex.Bootstrap.Wizzard(
    WizzardGraph(..)
  , WizzardStep(..)
  , wizzardStepName
  , wizzardStepBody
  , WizzardControlPos(..)
  , WizzardConfig(..)
  , wizzardConfigNextLabel
  , wizzardConfigBackLabel
  , wizzard
  ) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Dynamic (Typeable, toDyn, fromDynamic)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import GHC.Generics
import Reflex.Dom

import Web.Reflex.Bootstrap.Markup
import Web.Reflex.Bootstrap.Router
import Web.Reflex.Bootstrap.Utils

import qualified Data.Dynamic as D

-- | Allowed ways to compose wizzard steps. Allows only list-like graph for
-- wizzard.
data WizzardGraph t m a b where
  -- | Last (or the least) step of wizzard step
  WizzardSingle :: Typeable b => (a -> WizzardStep t m b) -> WizzardGraph t m a b
  -- | Chain steps in wizzard
  WizzardChain :: Typeable c => (a -> WizzardStep t m c) -> WizzardGraph t m c b -> WizzardGraph t m a b

-- | Descrption of widget step
data WizzardStep t m a = WizzardStep {
  _wizzardStepName :: Text -- ^ Name of the step displayed in pill body
  -- | Current value of result, when the value is Just, next button is enabled
  -- and event for immediate change to next step. Input argument is last seen
  -- state of the body when user wants to go back.
, _wizzardStepBody :: Maybe a -> m (Dynamic t (Maybe a), Event t a)
}
makeLenses ''WizzardStep

-- | Defines available position for control elements of wizzard
data WizzardControlPos = WizzardControlTop | WizzardControlBottom
  deriving (Generic, Eq, Ord, Enum, Bounded, Show, Read)

-- | Global configuration for wizzard widget
data WizzardConfig t = WizzardConfig {
  _wizzardConfigNextLabel  :: Dynamic t Text -- ^ Label for Next button
, _wizzardConfigBackLabel  :: Dynamic t Text -- ^ Label for Prev button
, _wizzardConfigControlPos :: WizzardControlPos -- ^ Define position for control (Next and Back) buttons
}
makeLenses ''WizzardConfig

instance Reflex t => Default (WizzardConfig t) where
  def = WizzardConfig {
      _wizzardConfigNextLabel = pure "Next"
    , _wizzardConfigBackLabel = pure "Back"
    , _wizzardConfigControlPos = WizzardControlBottom
    }

-- | Shortcut for intrnal state of wizzard's pill
type PillData t m a = (Text, m (Event t a, Route t m (Event t a)))

-- | Create wizzard from given wizzard step DSL, fire event on last screen with
-- result of last widget.
wizzard :: forall t m a . MonadWidget t m => WizzardConfig t -> WizzardGraph t m () a -> m (Event t a)
wizzard WizzardConfig{..} = fmap switchPromptlyDyn . route . wizzard' [] () Nothing
  where
    wizzard' :: forall b c . [PillData t m c] -> b -> Maybe D.Dynamic -> WizzardGraph t m b c -> m (Event t c, Route t m (Event t c))
    wizzard' steps curV mstate curGraph@(WizzardSingle mkStep) = do
      let step = mkStep curV
          curName = _wizzardStepName step
      pillsRoute <- wizzardHeader steps curName
      (nextE, backRoute, manualNextE, curValD) <- case _wizzardConfigControlPos of
        WizzardControlTop -> mdo
          (backRoute, nextE) <- wizzardControls steps curValD
          (curValD, manualNextE) <- el "div" $ _wizzardStepBody step $ join $ fmap fromDynamic mstate
          pure (nextE, backRoute, manualNextE, curValD)
        WizzardControlBottom -> do
          (curValD, manualNextE) <- el "div" $ _wizzardStepBody step $ join $ fmap fromDynamic mstate
          (backRoute, nextE) <- wizzardControls steps curValD
          pure (nextE, backRoute, manualNextE, curValD)
      let nextValE = leftmost [
              fmapMaybe id (current curValD `tag` nextE)
            , manualNextE
            ]
      pure (nextValE, pillsRoute <> backRoute)
    wizzard' steps curV mstate curGraph@(WizzardChain mkStep graph) = do
      let step = mkStep curV
          curName = _wizzardStepName step
      pillsRoute <- wizzardHeader steps curName
      (nextE, backRoute, manualNextE, curValD) <- case _wizzardConfigControlPos of
        WizzardControlTop -> mdo
          (backRoute, nextE) <- wizzardControls steps curValD
          (curValD, manualNextE) <- el "div" $ _wizzardStepBody step $ join $ fmap fromDynamic mstate
          pure (nextE, backRoute, manualNextE, curValD)
        WizzardControlBottom -> do
          (curValD, manualNextE) <- el "div" $ _wizzardStepBody step $ join $ fmap fromDynamic mstate
          (backRoute, nextE) <- wizzardControls steps curValD
          pure (nextE, backRoute, manualNextE, curValD)
      let nextValE = leftmost [
              fmapMaybe id (current curValD `tag` nextE)
            , manualNextE
            ]
      let myPill v = (curName, wizzard' steps curV (Just $ toDyn v) curGraph)
          nextRoute = Route $ (\v -> wizzard' (steps ++ [myPill v]) v Nothing graph) <$> nextValE
      pure (never, nextRoute <> pillsRoute <> backRoute)

    wizzardHeader :: [PillData t m c] -> Text -> m (Route t m (Event t c))
    wizzardHeader is curItem = elClass "ul" "nav nav-pills" $ do
      allr <- forM is $ \(name, mr) -> do
        e <- li . href . text $ name
        pure . Route $ const mr <$> e
      _ <- liClass "active" . href . text $ curItem
      pure $ mconcat allr

    wizzardControls :: [PillData t m c] -> Dynamic t (Maybe d) -> m (Route t m (Event t c), Event t ())
    wizzardControls steps curValD = do
      backRoute <- case steps of
        [] -> pure . Route $ never
        _ -> do
          let (_, mr) = last steps
          e <- primaryButton _wizzardConfigBackLabel
          pure . Route $ const mr <$> e
      curValD' <- delayPostBuild Nothing curValD
      nextE <- primaryRightButtonDisable _wizzardConfigNextLabel $ isJust <$> curValD'
      pure (backRoute, nextE)

-- | Primary button with disable flag
primaryRightButtonDisable :: MonadWidget t m => Dynamic t Text -> Dynamic t Bool -> m (Event t ())
primaryRightButtonDisable s enableD = fmap switchPromptlyDyn . widgetHoldDyn $ ffor enableD $ \v -> if v
  then do
    (e, _) <- elAttr' "a" [("class", "btn btn-raised pull-right btn-primary"), ("href", "javascript:void(0)")] $ dynText s
    return $ domEvent Click e
  else do
    elAttr "fieldset" [("disabled", "")] $ do
      elAttr "a" [("class", "btn btn-raised pull-right btn-primary"), ("href", "javascript:void(0)")] $ dynText s
      pure never
