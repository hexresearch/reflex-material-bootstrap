module Web.Reflex.Bootstrap.Utils(
    showt
  , whenJust
  , periodical
  , whenPrev
  , globalIdRef
  , genId
  , widgetHoldEvent
  , widgetHoldEvent'
  , foldEvent
  , approxEq
  , traceEventWidget
  , traceDynWidget
  , printTimestamp
  , widgetHoldDyn
  , mergeDynamic
  , delayPostBuild
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Text (Text, pack)
import Data.Time
import Reflex
import Reflex.Dom
import System.IO.Unsafe
import Control.Monad.Extra

import qualified Data.Text as T

import Web.Reflex.Bootstrap.Markup

-- | Display value as a text
showt :: Show a => a -> Text
showt = pack . show

-- | Emit event periodical
periodical :: MonadWidget t m => NominalDiffTime -> m (Event t ())
periodical dt = do
  t <- liftIO getCurrentTime
  tickE <- tickLossy dt t
  return $ void tickE

-- | Fires when the predicated returns 'True' on previous and current values of event
whenPrev :: MonadWidget t m => (a -> a -> Bool) -> Event t a -> m (Event t a)
whenPrev f ea = do
  bDyn <- foldDyn collect (Nothing, False) ea
  return $ fmapMaybe filterEv $ updated bDyn
  where
  collect !a (Nothing, _) = (Just a, True)
  collect !a (Just a', _) = let v = f a' a in v `seq` (Just a, v)

  filterEv (Just a, True) = Just a
  filterEv _ = Nothing

-- | Reference to global counter for unique id generation
globalIdRef :: IORef Int
globalIdRef = unsafePerformIO $ newIORef 0
{-# NOINLINE globalIdRef #-}

-- | Generate unique ids
genId :: MonadIO m => m Int
genId = liftIO $ do
  i <- readIORef globalIdRef
  modifyIORef' globalIdRef (+1)
  return i

-- | Wrapper around 'widgetHold' for widgets that produces events at output
widgetHoldEvent :: MonadWidget t m => m (Event t a) -> Event t (m (Event t a)) -> m (Event t a)
widgetHoldEvent initW we = do
  dynRes <- widgetHold initW we
  return $ switchPromptlyDyn dynRes

-- | Wrapper around 'widgetHold' for widgets that produces events at output
widgetHoldEvent' :: MonadWidget t m => Event t (m (Event t a)) -> m (Event t a)
widgetHoldEvent' we = do
  dynRes <- widgetHold (return never) we
  return $ switchPromptlyDyn dynRes

-- | Wrapper around 'foldDyn' to fold over events
foldEvent :: MonadWidget t m => (a -> b -> b) -> b -> Event t a -> m (Event t b)
foldEvent f b0 e = do
  dynB <- foldDyn f b0 e
  return $ updated dynB

-- | Approximately equality for floatings
approxEq :: (Num a, Ord a, Fractional a) => a -> a -> Bool
approxEq a b = abs (a - b) < 0.00001

-- | Display contents of value in info panel
traceEventWidget :: (Show a, MonadWidget t m) => Event t a -> m ()
traceEventWidget e = void . widgetHold (pure ()) $ info . showt <$> e

-- | Display contents of value in info panel
traceDynWidget :: (Show a, MonadWidget t m) => Dynamic t a -> m ()
traceDynWidget d = do
  let  d' = fmap (info . showt) d
  void . dyn $ d'

-- | Helper to print timestamp in ISO8601
printTimestamp :: UTCTime -> Text
printTimestamp = T.pack . formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S")

-- | Same as 'widgetHold' but for dynamic
widgetHoldDyn :: forall t m a . MonadWidget t m => Dynamic t (m a) -> m (Dynamic t a)
widgetHoldDyn maD = do
  ma <- sample . current $ maD
  widgetHold ma $ updated maD

-- | Merge two dynamics. If first dynamic changes, use it value as final, if second dynamic changes, use it value.
--
-- Note: this is helpful for creating local data loops, when second dynamic is produced from the first dynamic.
mergeDynamic :: forall t m a . MonadWidget t m => Dynamic t a -> Dynamic t a -> m (Dynamic t a)
mergeDynamic d1 d2 = do
  v0 <- sample . current $ d1
  holdDyn v0 $ leftmost [updated d1, updated d2]

-- | Break recursive dependency on value at build by replacing it with known default. Change back to current value of
-- dynamic after building.
delayPostBuild :: forall t m a . MonadWidget t m => a -> Dynamic t a -> m (Dynamic t a)
delayPostBuild defVal d = do
  buildE <- getPostBuild
  holdDyn defVal $ leftmost [updated d, d `tagPromptlyDyn` buildE]
