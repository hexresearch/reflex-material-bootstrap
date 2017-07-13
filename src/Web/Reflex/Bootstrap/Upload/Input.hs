{-|
Module      : Web.Reflex.Bootstrap.Upload.Input
Description : Reading files with HTML5 File API
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable

TODO: pull request this to reflex-dom-contrib
-}
module Web.Reflex.Bootstrap.Upload.Input(
    UploadFileConfig(..)
  , defaultUploadFileConfig
  , UploadFile(..)
  , uploadFileInput
  , debugUploadFile
  , FullUploadFile(..)
  , uploadFullFileInput
  , uploadJsonFileInput
  ) where

import Control.Exception (finally)
import Control.Monad.IO.Class
import Data.JSString (unpack)
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import GHC.Generics
import GHCJS.Buffer
import GHCJS.DOM.File (File, getName)
import GHCJS.Foreign.Callback
import GHCJS.Types (JSString, JSVal)
import JavaScript.TypedArray.ArrayBuffer
import Reflex
import Reflex.Dom

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Web.Reflex.Bootstrap.Utils

-- | Additional configuration for upload file input widget
data UploadFileConfig t = UploadFileConfig {
    uploadFileInputAttrs :: Dynamic t (Map Text Text)
  }

-- | Default configuration
defaultUploadFileConfig :: Reflex t => UploadFileConfig t
defaultUploadFileConfig = UploadFileConfig (constDyn mempty)

-- | Info about file being uploaded
data UploadFile t m = UploadFile {
  uploadFileName :: !Text -- ^ Selected file name
, uploadFileType :: !Text -- ^ Example: 'text/plain'
, uploadFileSize :: !Word -- ^ Total size of file
, uploadFileContent :: !(Event t (Word, Word) -> m (Event t BS.ByteString))
  -- ^ Getter of file contents, takes start index and end index (not including the end)
} deriving (Generic)

-- | Info about file fully loaded in memory
data FullUploadFile a = FullUploadFile {
  uploadFullFileName    :: !Text -- ^ Selected file name
, uploadFullFileType    :: !Text -- ^ Example: 'text/plain'
, uploadFullFileSize    :: !Word -- ^ Total size of file
, uploadFullFileContent :: !a -- ^ Full contents of file
} deriving (Generic)

-- | Typed wrapper around js FileReader object
newtype FileReader = FileReader JSVal
-- | Typed wrapper around js event passed into load callback of FileReader
newtype OnLoadEvent = OnLoadEvent JSVal

foreign import javascript unsafe "$r = new FileReader();"
  js_newFileReader :: IO FileReader
foreign import javascript unsafe "$1.onload = $2;"
  js_readerOnload :: FileReader -> Callback (JSVal -> IO ()) -> IO ()
foreign import javascript unsafe "$r = $1.target.result;"
  js_onLoadEventArrayBuffer :: OnLoadEvent -> IO ArrayBuffer
foreign import javascript unsafe "$1.readAsArrayBuffer($2);"
  js_readAsArrayBuffer :: FileReader -> File -> IO ()

foreign import javascript unsafe "$r = $1.type;"
  js_fileType :: File -> IO JSString
foreign import javascript unsafe "$r = $1.size;"
  js_fileSize :: File -> IO Word
foreign import javascript unsafe "$r = $1.slice($2, $3);"
  js_fileSlice :: File -> Word -> Word -> IO File

-- | Input for JSON file that is deserialised to specified type
uploadJsonFileInput :: forall a t m . (A.FromJSON a, MonadWidget t m)
  => UploadFileConfig t -- ^ Configuration of the widget
  -> m (Event t (Either Text (FullUploadFile a)))
uploadJsonFileInput cfg = do
  eu <- uploadFullFileInput cfg
  return $ ffor eu $ \file@FullUploadFile{..} -> case A.eitherDecode' $ BSL.fromStrict uploadFullFileContent of
    Left er -> Left (showt er)
    Right a -> Right $ file { uploadFullFileContent = a }

-- | Simplification of 'uploadFileInput' that loads file in memory instantly
uploadFullFileInput :: forall t m . MonadWidget t m => UploadFileConfig t
  -> m (Event t (FullUploadFile BS.ByteString))
uploadFullFileInput cfg = do
  eu <- uploadFileInput cfg
  fmap switchPromptlyDyn $ widgetHold (pure never) $ ffor eu $ \UploadFile{..} -> do
    let makeFullFile bs = FullUploadFile {
            uploadFullFileName    = uploadFileName
          , uploadFullFileType    = uploadFileType
          , uploadFullFileSize    = uploadFileSize
          , uploadFullFileContent = bs
          }
    buildE <- getPostBuild
    cntE <- uploadFileContent $ const (0, uploadFileSize) <$> buildE
    return $ makeFullFile <$> cntE

-- | Single file input that returns lazy byte string of file content
uploadFileInput :: forall t m . MonadWidget t m => UploadFileConfig t
  -> m (Event t (UploadFile t m))
uploadFileInput UploadFileConfig{..} = do
  i <- genId
  let inputId = "fileinput" <> showt i
      attrs = fmap (M.insert "id" inputId) uploadFileInputAttrs
      cfg = FileInputConfig attrs
  FileInput{..} <- fileInput cfg
  let filesEvent = updated _fileInput_value
  performEventAsync (readUploadFiles <$> filesEvent)
  where
  readUploadFiles :: [File] -> (UploadFile t m -> IO ()) ->  WidgetHost m ()
  readUploadFiles files consume = mapM_ (readUploadFile consume) files

  readUploadFile :: (UploadFile t m -> IO ()) -> File -> WidgetHost m ()
  readUploadFile consume f = liftIO $ do
    name <- T.pack . unpack <$> getName f
    ftype <- T.pack . unpack <$> js_fileType f
    size <- js_fileSize f
    consume $ UploadFile {
        uploadFileName = name
      , uploadFileType = ftype
      , uploadFileSize = size
      , uploadFileContent = contentGetter
      }
    where
    contentGetter sliceE = performEventAsync $ ffor sliceE
      $ \(start, end) consumeChunk -> liftIO $ do
        f' <- js_fileSlice f start end
        reader <- js_newFileReader
        rec c <- syncCallback1 ContinueAsync (onload c consumeChunk)
        js_readerOnload reader c
        js_readAsArrayBuffer reader f'

    onload c consumeChunk e = finally (releaseCallback c) $ do
      contentsBuff <- js_onLoadEventArrayBuffer $ OnLoadEvent e
      consumeChunk $ toByteString 0 Nothing $ createFromArrayBuffer contentsBuff

-- | Showcase for upload file input widget
debugUploadFile :: forall t m . MonadWidget t m => m ()
debugUploadFile = do
  fileE <- uploadFileInput defaultUploadFileConfig
  _ <- widgetHold (pure ()) $ renderFile <$> fileE
  return ()
  where
  renderFile :: UploadFile t m -> m ()
  renderFile UploadFile{..} = el "div" $ do
    el "p" $ text $ "Name: " <> uploadFileName
    el "p" $ text $ "Type: " <> uploadFileType
    el "p" $ text $ "Size: " <> showt uploadFileSize

    initE <- getPostBuild
    contentE <- uploadFileContent $ const (0, 10) <$> initE
    _ <- widgetHold (pure ()) $ ffor contentE $ \bs ->
      el "p" $ text $ "First bytes: " <> showt bs
    return ()
