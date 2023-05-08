module Verda.Asset.TestUtils where

import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.Reader    (ReaderT(runReaderT), MonadReader (ask), MonadIO)
import           Verda.Asset.Internal
import           Verda.Asset.Types

newtype TestReader a = TestReader {unContext :: ReaderT (Assets TestReader) IO a} deriving (Applicative, Functor, Monad, MonadIO, MonadReader (Assets TestReader), MonadUnliftIO)

instance AssetReader TestReader where
    askAssets = ask

withAssets :: Assets TestReader -> TestReader () -> IO ()
withAssets assets = flip runReaderT assets . unContext

withDefaultAssets :: TestReader () -> IO ()
withDefaultAssets action = mkDefaultAssets >>= flip withAssets action