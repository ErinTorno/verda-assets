module Verda.Asset.MetaSpec where

import           Data.Default
import           Data.Function            ((&))
import qualified Data.HashMap.Strict      as HM
import           Data.Proxy               (Proxy(..))
import           Test.Hspec
import           Verda.Asset.Meta

spec :: Spec
spec = do
    lengthSpec
    lookupSpec
    metaUnionSpec

lengthSpec :: Spec
lengthSpec =
    context "MetaMap.length" do
        it "should be 0 for def" do
            let meta :: MetaMap = def
            meta.length `shouldBe` 0
            meta.lengthOf (Proxy :: Proxy ()) `shouldBe` 0
        it "should return sum of 1 type" do
            let meta = def & withMeta @Int "a" 123 & withMeta @Int "b" 456 & withMeta @Int "c" 789
            meta.length `shouldBe` 3
            meta.lengthOf (Proxy :: Proxy Int) `shouldBe` 3
        it "should return sum across all types" do
            let meta = def
                     & withMeta @Int "a" 123 & withMeta @Int "b" 456 & withMeta @Int "c" 789
                     & withMeta "active?" True & withMeta "named?" False
                     & withMeta "tags"   (["enemy", "flying"] :: [String])
                     & withMeta "points" ([(0, 1), (8, 17), (3, 1)] :: [(Int, Int)])
            meta.length `shouldBe` 7
            meta.lengthOf (Proxy :: Proxy Int) `shouldBe` 3
            meta.lengthOf (Proxy :: Proxy Bool) `shouldBe` 2
            meta.lengthOf (Proxy :: Proxy [String]) `shouldBe` 1
            meta.lengthOf (Proxy :: Proxy [(Int, Int)]) `shouldBe` 1

lookupSpec :: Spec
lookupSpec =
    context "MetaMap.lookup" do
        it "should return Nothing for any missing" do
            let meta :: MetaMap = def
            meta.lookup "id"    `shouldBe` (Nothing :: Maybe Int)
            meta.lookup "tags"  `shouldBe` (Nothing :: Maybe String)
            meta.lookup "verts" `shouldBe` (Nothing :: Maybe [(Double, Double)])
            meta.lookupAllOf (Proxy :: Proxy Int) `shouldBe` HM.empty
        it "should return valid values of any type" do
            let verts :: [(Double, Double)] = [(0, 1.5), (18.25, 23)]
                name :: String = "Hefernetica"
                meta = def
                     & withMeta @Int "id" 123 & withMeta @Int "version" 1
                     & withMeta @String "id" "ety#123"
                     & withMeta "name" name
                     & withMeta "verts" verts
            meta.lookup "id"      `shouldBe` Just (123 :: Int)
            meta.lookup "id"      `shouldBe` Just ("ety#123" :: String)
            meta.lookup "version" `shouldBe` (Just 1 :: Maybe Int)
            meta.lookup "verts"   `shouldBe` Just verts
            meta.lookup "name"    `shouldBe` Just name
            meta.lookupAllOf (Proxy :: Proxy Int) `shouldBe` HM.fromList [("id", 123), ("version", 1)] 

metaUnionSpec :: Spec
metaUnionSpec =
    context "metaUnion" do
        it "shouldn't be changed when merging with empty" do
            let meta = def & withMeta @Int "a" 123
            (metaUnion meta mempty).length `shouldBe` 1
            (metaUnion mempty meta).length `shouldBe` 1
        it "should merge two maps with distinct keys" do
            let ma = def & withMeta @Int "x" 1 & withMeta @Int "y" 10
                mb = def & withMeta "x" False
                mu = metaUnion ma mb
            mu.length `shouldBe` 3
        it "should keep values from the second map if the same key-type is in both" do
            let ma = def & withMeta @Int "x" 1 & withMeta @Int "y" 10
                mb = def & withMeta @Int "x" 9
                mu = metaUnion ma mb
            mu.length `shouldBe` 2
            mu.lookup "x" `shouldBe` Just (9 :: Int)
            (mb <> ma).lookup "x" `shouldBe` Just (1 :: Int)