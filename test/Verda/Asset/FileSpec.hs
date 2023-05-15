module Verda.Asset.FileSpec where

import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Maybe             (isJust)
import           Test.Hspec
import           Verda.Asset.Types
import           Verda.Asset.TestUtils

spec :: Spec
spec = do
    fileAssetIOSpec

fileAssetIOSpec :: Spec
fileAssetIOSpec =
    context "instance AssetIO m FileAssetIO" do
        it "should return Nothing when file isn't found" do
            withDefaultAssets do
                assets <- askAssets
                bytes  <- assets.assetIO.loadFile "./this file should not exist ;)"
                liftIO do
                    bytes `shouldBe` Nothing
        it "should return file bytes when file exists" do
            withDefaultAssets do
                assets <- askAssets
                bytes  <- assets.assetIO.loadFile "./verda-assets.cabal"
                liftIO do
                    bytes `shouldSatisfy` isJust
