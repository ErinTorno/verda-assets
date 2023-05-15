module Verda.Asset.File where

import           Control.Exception
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString        as BS
import           System.Directory       (doesPathExist, doesFileExist)
import           Verda.Asset.Types      (AssetIO(..))

fileAssetIO :: MonadIO m => AssetIO m
fileAssetIO = AssetIO
    { loadFile = \file -> liftIO $
        (Just <$> BS.readFile file) `catch` \(_ :: IOException) -> pure Nothing
    , isValidFile = liftIO . doesFileExist
    , isValidPath = liftIO . doesPathExist
    }