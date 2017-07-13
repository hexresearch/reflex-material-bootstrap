{-|
Module      : Web.Reflex.Pagination
Description : Helpers to make pagination widgets
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Web.Reflex.Bootstrap.Pagination(
    renderList
  , renderListReload
  , renderPage
  , renderPageReload
  , renderPager
  , PagedList(..)
  , Page
  ) where

import GHC.Generics
import Reflex as R
import Reflex.Dom as R
import Web.Reflex.Bootstrap.Utils

-- | List with total number of elements
data PagedList a = PagedList {
  pagedListItems :: ![a]
, pagedListTotal :: !Word
} deriving (Generic, Show, Functor)

type Page = Word

-- | Widget that renders remote data as list with pagination controls
renderList :: forall t m a b . MonadWidget t m
  => Maybe Word -- ^ How much pages to display by one side of current page, 'Nothing' means all
  -> (forall c . m c -> m c) -- ^ Wrapper around whole body
  -> (a -> m b) -- ^ Renderer of item
  -> (Event t Page -> m (Event t (PagedList a))) -- ^ Getter of pages
  -> m (Dynamic t [b])
renderList maxPages bodyWrap render req = renderListReload maxPages bodyWrap render req never

-- | Widget that renders remote data as list with pagination controls
renderListReload :: forall t m a b c . MonadWidget t m
  => Maybe Word -- ^ How much pages to display by one side of current page, 'Nothing' means all
  -> (forall d . m d -> m d) -- ^ Wrapper around whole body
  -> (a -> m b) -- ^ Renderer of item
  -> (Event t Page -> m (Event t (PagedList a))) -- ^ Getter of pages
  -> Event t c -- ^ Reload event
  -> m (Dynamic t [b])
renderListReload maxPages bodyWrap render = renderPageReload maxPages [] render'
  where
  render' :: Page -> PagedList a -> m [b]
  render' _ (PagedList datum _) = bodyWrap $ mapM render datum

-- | Widget that renders remote data as pages with pagination controls
renderPage :: forall t m a b . MonadWidget t m
  => Maybe Word -- ^ How much pages to display by one side of current page, 'Nothing' means all
  -> b -- ^ Default value for page (while not loaded)
  -> (Page -> PagedList a -> m b) -- ^ Renderer of page
  -> (Event t Page -> m (Event t (PagedList a))) -- ^ Getter of pages
  -> m (Dynamic t b)
renderPage maxPages defb render getPage = renderPageReload maxPages defb render getPage never

-- | Widget that renders remote data as pages with pagination controls
renderPageReload :: forall t m a b c . MonadWidget t m
  => Maybe Word -- ^ How much pages to display by one side of current page, 'Nothing' means all
  -> b -- ^ Default value for page (while not loaded)
  -> (Page -> PagedList a -> m b) -- ^ Renderer of page
  -> (Event t Page -> m (Event t (PagedList a))) -- ^ Getter of pages
  -> Event t c -- ^ Reload event
  -> m (Dynamic t b)
renderPageReload maxPages defb render getPage reloadEvent = do
  initE <- fmap (const 0) <$> getPostBuild
  rec (pageE, es) <- pager $ leftmost [initE, pageE, const 0 <$> reloadEvent]
  return es
  where
  pager :: Event t Page -> m (Event t Page, Dynamic t b)
  pager curw = do
    curwD <- holdDyn 0 curw
    edata <- getPage curw
    let edataWithPage = attachPromptlyDynWith (,) curwD edata
    dynRes <- widgetHold (pure (never, defb)) $ uncurry renderContent <$> edataWithPage
    let (pageED, esD) = splitDynPure dynRes
    return (switchPromptlyDyn pageED, esD)

  renderContent :: Page -> PagedList a -> m (Event t Page, b)
  renderContent curw pl@(PagedList _ w) = do
    pageE <- renderPager maxPages curw w
    es <- render curw pl
    return (pageE, es)

-- | Display pager widget that reacts to clicking on it
renderPager :: forall t m . MonadWidget t m
  => Maybe Word -- ^ How much pages to display by one side of current page, 'Nothing' means all
  -> Page -- ^ Current page
  -> Word -- ^ Maximum count of pages
  -> m (Event t Page) -- Returns event of next page requested by user
renderPager maxSide curw pages' = do
  elAttr "nav" navAttrs $ elAttr "div" pagAttrs $ elClass "ul" "pagination" $ do
    begE <- beginButton
    prevE <- prevButton
    pagesE <- mapM pageButton pagesRange
    nextE <- nextButton
    endE <- endButton
    return $ leftmost $ [begE, endE, prevE, nextE] ++ pagesE
  where
  pages = max 1 pages' -- zero pages leads to strange behavior

  navAttrs = [("aria-label", "Items navigation"), ("style", "text-align: center;")]
  pagAttrs = [("style", "display: inline-block")]
  pagesRange = case maxSide of
    Nothing -> [0 .. pages-1]
    Just n -> [i | i <- [0 .. pages-1], fromIntegral i > mib n, fromIntegral i < mab n]
    where
      mib, mab :: Word -> Int
      mib n = fromIntegral curw - fromIntegral n
      mab n = fromIntegral curw + fromIntegral n

  beginButton :: m (Event t Page)
  beginButton
    | curw == 0 = do
      elClass "li" "disabled" $ el "a" $ text "««"
      return never
    | otherwise = el "li" $ do
      (e, _) <- elAttr' "a" [("href", "#")] $ text "««"
      return $ const 0 <$> domEvent Click e

  prevButton :: m (Event t Page)
  prevButton
    | curw == 0 = do
      elClass "li" "disabled" $ el "a" $ text "«"
      return never
    | otherwise = el "li" $ do
      (e, _) <- elAttr' "a" [("href", "#")] $ text "«"
      return $ const (curw-1) <$> domEvent Click e

  nextButton :: m (Event t Page)
  nextButton
    | curw == pages-1 = do
      elClass "li" "disabled" $ el "a" $ text "»"
      return never
    | otherwise = el "li" $ do
      (e, _) <- elAttr' "a" [("href", "#")] $ text "»"
      return $ const (curw+1) <$> domEvent Click e

  endButton :: m (Event t Page)
  endButton
    | curw == pages-1 = do
      elClass "li" "disabled" $ el "a" $ text "»»"
      return never
    | otherwise = el "li" $ do
      (e, _) <- elAttr' "a" [("href", "#")] $ text "»»"
      return $ const (pages-1) <$> domEvent Click e

  pageButton :: Page -> m (Event t Page)
  pageButton i
    | i == curw = do
      elClass "li" "active" $ el "a" $ text (showt $ i+1)
      return never
    | otherwise = el "li" $ do
      (e, _) <- elAttr' "a" [("href", "#")] $ text (showt $ i+1)
      return $ const i <$> domEvent Click e
