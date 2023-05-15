{-# LANGUAGE UndecidableInstances #-}

module Verda.Asset.Meta where

import           Data.Default                  (Default(..))
import           Data.Kind                     (Type)
import           Data.Text                     (Text)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HM
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Proxy                    (Proxy(..))
import           Data.TypeMap.Dynamic.Alt      (TypeMap)
import qualified Data.TypeMap.Dynamic.Alt      as TM
import qualified Data.TypeMap.Internal.Dynamic as TM.Internal
import           GHC.Exts                      (Any)
import           GHC.Records                   (HasField(..))
import           Type.Reflection               (Typeable)
import           Verda.Asset.Internal.Impl     (Impl)

data MetaS
type instance TM.Item MetaS t = HashMap Text t

newtype MetaMap = MetaMap {unwrap :: TypeMap MetaS}

instance Default MetaMap where
    def = MetaMap TM.empty

instance Semigroup MetaMap where
    (<>) = metaUnion

instance Monoid MetaMap where
    mempty = def

instance HasField "length" MetaMap Int where
    getField mm = tmFold (\_ acc hm -> acc + HM.size hm) 0 mm.unwrap

instance forall (a :: Type). (Impl a, Typeable a) => HasField "lengthOf" MetaMap (Proxy a -> Int) where
    getField mm proxy = metaLengthOf proxy mm

instance forall a. (Impl a, Typeable a) => HasField "lookup" MetaMap (Text -> Maybe a) where
    getField = flip metaLookup

instance forall a. (Impl a, Typeable a) => HasField "lookupAllOf" MetaMap (Proxy a -> HashMap Text a) where
    getField mm _ = metaLookupAll mm

withMeta :: forall mt. Typeable mt => Text -> mt -> MetaMap -> MetaMap
withMeta label m mm = MetaMap $ TM.alter @mt alt mm.unwrap
    where alt (Just hm) = Just $ HM.insert label m hm
          alt Nothing   = Just $ HM.singleton label m

metaLength :: MetaMap -> Int
metaLength mm = mm.length

metaLookup :: forall a. Typeable a => Text -> MetaMap -> Maybe a
metaLookup label mm = TM.lookup @a mm.unwrap >>= HM.lookup label

metaLookupAll :: forall a. Typeable a => MetaMap -> TM.Item MetaS a
metaLookupAll mm = fromMaybe mempty $ TM.lookup @a mm.unwrap

metaLengthOf :: forall (a :: Type). Typeable a => Proxy a -> MetaMap -> Int
metaLengthOf _ mm = maybe 0 HM.size $ TM.lookup @a mm.unwrap

-- | The union of two MetaMaps; if the same key for the same type is present in both, it keeps the second's
metaUnion :: MetaMap -> MetaMap -> MetaMap
metaUnion ma mb =
    let TM.Internal.TypeMap a = ma.unwrap
        TM.Internal.TypeMap b = mb.unwrap
        union :: TM.Item MetaS t -> TM.Item MetaS t -> TM.Item MetaS t
        union = flip HM.union
        merge = TM.Internal.withTypeRep (const union) (Proxy :: Proxy (ItemUnionWith MetaS))
     in MetaMap $ TM.Internal.TypeMap $ Map.unionWithKey merge a b

-------------------
-- TypeMap Utils --
-------------------

tmFold :: forall a x. (forall t. Typeable t => Proxy t -> a -> TM.Item x t -> a) -> a -> TM.TypeMap x -> a
tmFold f a (TM.Internal.TypeMap m) = Map.foldlWithKey' f' a m
    where f' acc k = TM.Internal.withTypeRep f (Proxy :: Proxy (ItemFold x a)) k acc

data ItemFold x a
type instance TM.Internal.Typed (ItemFold x a) t = a -> TM.Item x t -> a
type instance TM.Internal.UnTyped (ItemFold x a) = a -> Any -> a

data ItemUnionWith x
type instance TM.Internal.Typed (ItemUnionWith x) t = TM.Item x t -> TM.Item x t -> TM.Item x t
type instance TM.Internal.UnTyped (ItemUnionWith x) = Any -> Any -> Any