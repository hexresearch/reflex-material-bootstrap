{-# LANGUAGE TemplateHaskell #-}
module Web.Reflex.Bootstrap.Progress(
    ProgressConfig(..)
  , progressStripped
  , progressAnimated
  , progressBars
  , progressAttrs
  , ProgressBarConfig(..)
  , progressBarContext
  , progressBarValue
  , progressBarAttrs
  , ProgressWidget(..)
  , progressWidgetFinish
  , progressWidgetElement
  , progressWidgetBars
  , progressWidget
  , timeProgressBar
  ) where

import Control.Lens.TH
import Data.Default
import Data.Functor
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Reflex.Dom

import Web.Reflex.Bootstrap.Context
import Web.Reflex.Bootstrap.Utils

import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | Configuration for single bar in 'ProgressConfig'
data ProgressBarConfig t = ProgressBarConfig {
  _progressBarContext  :: !(Dynamic t ComponentContext) -- ^ Which color context to use
, _progressBarValue    :: !(Dynamic t Double) -- ^ Value to display from 0.0 to 1.0
, _progressBarAttrs    :: !(Dynamic t (Map Text Text)) -- ^ Additional attributes
} deriving (Generic)
makeLenses ''ProgressBarConfig

instance Reflex t => Default (ProgressBarConfig t) where
  def = ProgressBarConfig {
      _progressBarContext = pure NoContext
    , _progressBarValue = pure 0.35
    , _progressBarAttrs = pure []
    }

-- | Configuration for progress bar widget
data ProgressConfig t = ProgressConfig {
  _progressStripped :: !(Dynamic t Bool) -- ^ Use stripped style?
, _progressAnimated :: !(Dynamic t Bool) -- ^ Use animated style?
, _progressBars     :: ![ProgressBarConfig t] -- ^ Configuration for actual bars, many bars == stacked progress bar
, _progressAttrs    :: !(Dynamic t (Map Text Text)) -- ^ Additional attributes
} deriving (Generic)
makeLenses ''ProgressConfig

instance Reflex t => Default (ProgressConfig t) where
  def = ProgressConfig {
      _progressStripped = pure False
    , _progressAnimated = pure False
    , _progressBars = [def]
    , _progressAttrs = pure []
    }

-- | Created progress bar widget
data ProgressWidget t = ProgressWidget {
  _progressWidgetFinish  :: Event t () -- ^ Fired when progress bar becomes >= 1.0
, _progressWidgetElement :: Element EventResult GhcjsDomSpace t -- ^ Created outer raw element
, _progressWidgetBars    :: [Element EventResult GhcjsDomSpace t] -- ^ Inner elements for bars
} deriving (Generic)
makeLenses ''ProgressWidget

-- | Create progress bar widget
progressWidget :: forall t m . MonadWidget t m => ProgressConfig t -> m (ProgressWidget t)
progressWidget ProgressConfig{..} = do
  let outerAttrs = do
        isStripped <- _progressStripped
        isAnimated <- _progressAnimated
        atrs <- _progressAttrs
        let active = if isAnimated then "active" else ""
            stripped = if isStripped then "progress-stripped" else ""
            ourClasses = T.unwords ["progress", active, stripped]
            classAtr = case M.lookup "class" atrs of
              Nothing -> ourClasses
              Just s -> ourClasses <> " " <> s
        pure $ M.insert "class" classAtr atrs
  let makeBar ProgressBarConfig{..} = do
        let innerAttrs = do
              atrs <- _progressBarAttrs
              cntx <- _progressBarContext
              val <- _progressBarValue
              let cntxClass = case cntx of
                    NoContext -> ""
                    InfoContext -> "progress-bar-info"
                    SuccessContext -> "progress-bar-success"
                    WarningContext -> "progress-bar-warning"
                    DangerContext -> "progress-bar-danger"
                  ourClasses = "progress-bar " <> cntxClass
                  valStyle = "width: " <> showt (100 * val) <> "%;"
                  classAtr = case M.lookup "class" atrs of
                    Nothing -> ourClasses
                    Just s -> ourClasses <> " " <> s
                  sytleAtr = case M.lookup "style" atrs of
                    Nothing -> valStyle
                    Just s -> valStyle <> " " <> s
              pure $ M.insert "class" classAtr . M.insert "style" sytleAtr $ atrs
        (elV, _) <- elDynAttr' "div" innerAttrs $ pure ()
        pure elV
  (outerEl, bars) <- elDynAttr' "div" outerAttrs $ traverse makeBar _progressBars
  let totalD = fmap sum . sequence . fmap _progressBarValue $ _progressBars
  pure ProgressWidget {
      _progressWidgetFinish = void . ffilter (>= 1.0) $ updated totalD
    , _progressWidgetElement = outerEl
    , _progressWidgetBars = bars
    }

-- | Create progress bar that getting finished after given amount of time. Uses the
-- first bar in configuration.
timeProgressBar :: forall t m . MonadWidget t m
  => NominalDiffTime -- ^ Time after that the progress should be filled
  -> NominalDiffTime -- ^ Interval to update the progress bar
  -> ProgressConfig t -> m (ProgressWidget t)
timeProgressBar t dt cfg@ProgressConfig{..} = case _progressBars of
  [] -> do
    te <- headE =<< periodical t
    w <- progressWidget cfg
    pure w { _progressWidgetFinish = te }
  bcfg@ProgressBarConfig{..} : _ -> do
    dte <- periodical dt
    let n = realToFrac t / realToFrac dt :: Double
    valD <- foldDyn (\_ b -> b + 1 / n) 0 dte
    w <- progressWidget cfg {
        _progressBars = [bcfg {
            _progressBarValue = valD
          }]
      }
    finishE <- headE $ _progressWidgetFinish w
    pure w { _progressWidgetFinish = finishE }
