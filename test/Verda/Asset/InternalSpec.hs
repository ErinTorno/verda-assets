module Verda.Asset.InternalSpec where

import           Control.Monad.IO.Class   (MonadIO(liftIO))
import           Control.Monad            (forM_)
import           Control.Concurrent.MVar  (swapMVar)
import           Control.Exception        (Exception, throwIO)
import qualified Data.ByteString          as BS
import           Data.Default             (def)
import           Data.Functor
import           Data.Function            ((&))
import qualified Data.HashSet as HS
import           Data.Maybe
import           Data.String              (IsString)
import qualified Data.Text                as T
import           Data.Text.Encoding
import           Test.Hspec
import           Verda.Asset
import           Verda.Asset.Internal
import           Verda.Asset.Types
import           Verda.Asset.TestUtils

data MyAsset = MyAsset deriving (Eq, Ord, Show)

myAssetLoader :: Applicative m => AssetLoader m MyAsset
myAssetLoader = AssetLoader
    { extensions = HS.fromList ["", "myfile"]
    , load       = \_file _bytes -> pure $ Right $ justAsset MyAsset
    }

myLoadAliasLoader :: AssetLoader TestReader MyAsset
myLoadAliasLoader = AssetLoader
    { extensions = HS.singleton "alias"
    , load       = \_file bytes -> do
        let path = decodeUtf8Lenient bytes
        alias :: Handle MyAsset <- loadHandle (T.unpack path)
        pure $ Right $ justAlias alias
    }

data LoadShouldFail = LoadShouldFail deriving (Eq, Show)
instance Exception LoadShouldFail

myFailingLoader :: AssetLoader TestReader MyAsset
myFailingLoader = AssetLoader
    { extensions = HS.singleton ""
    , load       = \_file _bytes -> liftIO $ throwIO LoadShouldFail
    }

newtype ComplexAsset = ComplexAsset
    { simple :: Handle MyAsset
    } deriving (Eq, Show)

complexAssetLoader :: AssetLoader TestReader ComplexAsset
complexAssetLoader = AssetLoader
    { extensions = HS.singleton ""
    , load       = \_file bytes -> do
        simple :: Handle MyAsset <- loadHandle "./LICENSE"
        pure $ Right LoadContext
            { primary   = PKAsset ComplexAsset {..}
            , waitingOn = def & withHandle simple
            , meta      = noMeta & withMeta "bytelen" (BS.length bytes)
            }
    }

-----------
-- Tests --
-----------

spec :: Spec
spec = do
    loadHandleSpec
    loadPrimaryAssetSpec
    updateWODQueueSpec

loadHandleSpec :: Spec
loadHandleSpec =
    context "loadHandle" do
        it "should set Failed status for missing asset loader" do
            withDefaultAssets do
                handle :: Handle MyAsset <- loadHandle "any.myfile"
                status <- assetStatus handle
                liftIO do
                    handle.index `shouldBe` 0
                    status `shouldBe` Failed "No asset loader for type MyAsset"
        it "should set Failed status for no matching file extension" do
            assets <- mkDefaultAssets <&> withLoader myAssetLoader
            withAssets assets do
                handle :: Handle MyAsset <- loadHandle "any.rong"
                status <- assetStatus handle
                liftIO do
                    handle.index `shouldBe` 0
                    status `shouldBe` Failed "No asset loader for type MyAsset, file extension .rong"
        it "should queue for loading when doesn't exist for valid loader" do
            assets <- mkDefaultAssets <&> withLoader myAssetLoader
            withAssets assets do
                handle :: Handle MyAsset <- loadHandle "any.myfile"
                status <- assetStatus handle
                liftIO do
                    handle.index `shouldBe` 0
                    status `shouldBe` Loading
        it "should fetch current value if already loading" do
            assets <- mkDefaultAssets <&> withLoader myAssetLoader
            withAssets assets do
                let file = "a/long/path/to/this.myfile"
                handleA :: Handle MyAsset <- loadHandle file
                handleB :: Handle MyAsset <- loadHandle file
                liftIO do
                    handleA `shouldBe` handleB
        it "successive assets should have incrementing ids" do
            assets <- mkDefaultAssets <&> withLoader myAssetLoader
            withAssets assets do
                forM_ [0..1023] \(i :: Int) -> do
                    handle :: Handle MyAsset <- loadHandle $ "any#" <> show i <> ".myfile"
                    status <- assetStatus handle
                    liftIO do
                        handle.index `shouldBe` i
                        status `shouldBe` Loading

loadPrimaryAssetSpec :: Spec
loadPrimaryAssetSpec =
    context "loadPrimaryAsset" do 
        it "should load all existing loaded assets" do
            assets <- mkDefaultAssets <&> withLoader myAssetLoader
            withAssets assets do
                handle :: Handle MyAsset <- loadHandle "./LICENSE"
                toLoad <- liftIO $ swapMVar assets.queues.toLoad mempty
                forM_ toLoad loadPrimaryAsset
                status <- assetStatus handle
                asset  <- getAsset handle
                liftIO do
                    status `shouldBe` Loaded
                    asset  `shouldBe` Just MyAsset
        it "should update alias to be clone of original loaded" do
            assets <- mkDefaultAssets
                  <&> withLoader myLoadAliasLoader
                    . withLoader myAssetLoader
            withAssets assets do
                handle :: Handle MyAsset <- loadHandle "./LICENSE"
                alias  :: Handle MyAsset <- loadHandle "./test/assets/license.alias"
                
                toLoad <- liftIO $ swapMVar assets.queues.toLoad mempty
                forM_ toLoad loadPrimaryAsset
                attemptFinishAliases

                assetO  <- getAsset handle
                assetA  <- getAsset alias
                statusO <- assetStatus handle
                statusA <- assetStatus alias
                liftIO do
                    statusA `shouldBe` statusO
                    assetA  `shouldBe` assetO
                    assetO  `shouldBe` Just MyAsset
                    assetA  `shouldBe` Just MyAsset
        it "should set status to failed if file doesn't exist" do
            assets <- mkDefaultAssets <&> withLoader myAssetLoader
            withAssets assets do
                let file :: IsString a => a
                    file = " ./this file doesn't exist and should it ever I'm fine with the test breaking since that had to be intentional"
                handle :: Handle MyAsset <- loadHandle file
                toLoad <- liftIO $ swapMVar assets.queues.toLoad mempty
                forM_ toLoad loadPrimaryAsset
                status <- assetStatus handle
                asset  <- getAsset handle
                liftIO do
                    status `shouldBe` Failed ("File doesn't exist: \"" <> file <>"\"")
                    asset  `shouldBe` Nothing

updateWODQueueSpec :: Spec
updateWODQueueSpec =
    context "updateWODQueue" do
        it "should be stuck while dependencies are loading" do
            assets <- mkDefaultAssets
                  <&> withLoader complexAssetLoader
                    . withLoader myAssetLoader
            withAssets assets do
                handle :: Handle ComplexAsset <- loadHandle "./LICENSE"
                toLoad <- liftIO $ swapMVar assets.queues.toLoad mempty
                forM_ toLoad loadPrimaryAsset
                -- don't allow the dependencies to go through loadPrimaryAsset
                updateWODQueue

                status <- assetStatus handle
                asset  <- getAsset handle
                simpleHandle :: Handle MyAsset <- loadHandle "./LICENSE"
                simpleAsset  <- getAsset simpleHandle
                simpleStatus <- assetStatus simpleHandle
                liftIO do
                    status       `shouldBe` AwaitingDependencies
                    simpleStatus `shouldBe` Loading
                    asset        `shouldBe` Nothing
                    simpleAsset  `shouldBe` Nothing
        it "should load asset with dependencies and metadata" do
            assets <- mkDefaultAssets
                  <&> withLoader complexAssetLoader
                    . withLoader myAssetLoader
            withAssets assets do
                handle :: Handle ComplexAsset <- loadHandle "./LICENSE"
                let loadAssets = do
                        toLoad <- liftIO $ swapMVar assets.queues.toLoad mempty
                        forM_ toLoad loadPrimaryAsset
                loadAssets
                -- again for dependencies that loading Complex created
                loadAssets
                updateWODQueue

                status <- assetStatus handle
                asset  <- getAsset handle
                meta   <- assetMeta handle
                simpleHandle :: Handle MyAsset <- loadHandle "./LICENSE"
                liftIO do
                    status `shouldBe` Loaded
                    asset  `shouldBe` Just ComplexAsset {simple = simpleHandle}
                    metaLookup @Int "bytelen" meta `shouldBe` Just 1518
                let simple = (fromJust asset).simple
                simpleAsset  <- getAsset simple
                simpleStatus <- assetStatus simple
                liftIO do
                    simpleStatus `shouldBe` Loaded
                    simpleAsset `shouldBe` Just MyAsset