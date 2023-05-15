{-# LANGUAGE UnboxedTuples #-}

module Verda.Asset.Types where

import           Control.Concurrent.MVar      (MVar)
import           Control.Monad.ST             (RealWorld)
import           Data.ByteString              (ByteString)
import           Data.Default
import           Data.Dynamic                 (Dynamic, Typeable)
import           Data.Hashable                (Hashable(..))
import           Data.HashMap.Strict          (HashMap)
import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as HS
import           Data.HashTable.ST.Basic      (HashTable)
import           Data.Map.Strict              (Map)
import           Data.Sequence                (Seq)
import           Data.Text                    (Text)
import           Data.TypeMap.Dynamic.Alt     (TypeMap)
import qualified Data.TypeMap.Dynamic.Alt     as TM
import qualified Data.Vector.Mutable          as MVec
import           GHC.Generics                 (Generic)
import           System.Mem.Weak              (Weak)
import           Type.Reflection              (SomeTypeRep, typeRep)
import           Verda.Asset.Meta             (MetaMap)

-- | Status corresponding to the asset's presence in memory
data AssetStatus
    = Failed !Text
    -- ^ Asset failed to load
    | Unloaded
    -- ^ Asset lost all strong handles and was garbage collected
    | Loading
    -- ^ Asset is currently being loaded
    | AwaitingDependencies
    -- ^ Asset loaded, but it relies on other assets that are not yet loaded
    | Loaded
    -- ^ Asset and any dependencies have been loaded
    deriving (Eq, Generic, Ord, Read, Show)

data LoaderS m
type instance TM.Item (LoaderS m) t = HashMap Text (AssetLoader m t)
type LoaderMap m = TypeMap (LoaderS m)

data AssetRefS
type instance TM.Item AssetRefS t = AssetPathRef t
type AssetRefMap = TypeMap AssetRefS

data Assets m = Assets
    { aliases        :: !Aliases
    , assetIO        :: !(AssetIO m)
    , refsByPath     :: !(HashTable RealWorld FilePath AssetRefMap)
    , refsVector     :: !(MVar (MVec.IOVector AssetRefs))
    , loaders        :: !(LoaderMap m)
    , metaVector     :: !(MVar (MVec.IOVector MetaMap))
    , nextIndex      :: !(MVar Int)
    , queues         :: !(AssetQueues m)
    , settings       :: !AssetSettings
    , statuses       :: !(MVar (MVec.IOVector AssetStatus))
    , weakRefs       :: !(MVar (MVec.IOVector WeakAssetRefs))
    }

data AssetSettings = AssetSettings
    { assetDir       :: !FilePath -- ^ Relative directory from current dir where assets are stored (default "assets")
    , threadDelayMS  :: !Int      -- ^ How many milliseconds to sleep before attempting to load all queued assets again (default 15)
    , isHotReloading :: !Bool     -- ^ todo finish implementing :/ (default false)
    }

instance Default AssetSettings where
    def = AssetSettings
        { assetDir       = "assets"
        , threadDelayMS  = 15
        , isHotReloading = False
        }

class AssetReader m where
    askAssets :: m (Assets m)

data Aliases = Aliases
    { byOriginal :: !(MVar (HashMap Int (HashSet Int)))
    , byClone    :: !(MVar (HashMap Int Int))
    }

data AssetQueues m = AssetQueues
    { toFinishAliases :: !(MVar (Seq (AssetToLoad m, LoadContext Int)))
    , toFinishAssets  :: !(MVar (Seq (AssetToLoad m, LoadContext Dynamic)))
    , toLoad          :: !(MVar (Seq (AssetToLoad m)))
    }

-------------
-- Handles --
-------------

data HandleRef a
    = WeakRef   !(Weak (MVar a))
    -- ^ Warning: after unloading, previous Weak Handles will not be valid for a reloaded object!
    | StrongRef !(MVar a)

data Handle a = Handle
    { index :: {-# UNPACK #-} !Int
    , ref   :: !(HandleRef a)
    }

instance Hashable (Handle a) where
    hash h = 3 * h.index + case h.ref of WeakRef _ -> 1; _ -> 0 

    hashWithSalt s h = 3 * hashWithSalt s h.index + case h.ref of WeakRef _ -> 1; _ -> 0 

instance Eq (Handle a) where
    a == b = a.index == b.index && case (# a.ref, b.ref #) of
            (# WeakRef _,   WeakRef _   #) -> True
            (# StrongRef _, StrongRef _ #) -> True
            _                              -> False

instance Ord (Handle a) where
    a `compare` b = case a.index `compare` b.index of
        EQ -> case (# a.ref, b.ref #) of
            (# WeakRef _,   WeakRef _   #) -> EQ
            (# StrongRef _, StrongRef _ #) -> EQ
            (# WeakRef _,   _           #) -> LT
            (# StrongRef _, _           #) -> GT
        o -> o

instance Typeable a => Show (Handle a) where
    show handle = concat
        [ "Handle @", show (typeRep @a)
        , " {index = ", show handle.index
        , ", ref = ", case handle.ref of WeakRef _ -> "Weak"; StrongRef _ -> "Strong"
        , "}"
        ]

newtype HandleSet = HandleSet {unwrap :: HashSet Int} deriving (Eq, Ord, Hashable, Monoid, Semigroup, Show)

instance Default HandleSet where def = HandleSet HS.empty

-------------
-- Loading --
-------------

data PrimaryKind a
    = PKAsset   !a
    | PKAlias   !(Handle a)

data DynPrimaryKind
    = DynPKAsset !Dynamic -- ~ a
    | DynPKAlias !Int     -- Handle.index

data LoadContext a = LoadContext
    { primary   :: !a
    , waitingOn :: !HandleSet
    , meta      :: !MetaMap
    }

instance Functor LoadContext where
    fmap f LoadContext{..} = LoadContext
        { primary = f primary
        , ..
        }

type LoadSuccess a = LoadContext (PrimaryKind a)

data AssetLoader m a = AssetLoader
    { extensions :: HashSet Text
    , load       :: FilePath -> ByteString -> m (Either Text (LoadSuccess a))
    }

data DynAssetLoader m = DynAssetLoader
    { extensions :: HashSet Text
    , load       :: FilePath -> ByteString -> m (Either Text (LoadContext DynPrimaryKind))
    }

data AssetToLoad m = AssetToLoad
    { index  :: {-# UNPACK #-} !Int
    , loader :: !(DynAssetLoader m)
    , file   :: !FilePath
    , accept :: !(Dynamic -> IO ()) -- receives the final dynamic value to store in its MVar
    , acceptRefs :: !(AssetRefs -> IO ())
    }

----------------
-- Asset Refs --
----------------

data AssetRefs = AssetRefs
    { primary :: !Dynamic                -- Dynamic ~ Weak (MVar a)
    , labeled :: !(HashMap Text Dynamic) -- Dynamic ~ Weak (MVar a)
    , index   :: !Int
    , meta    :: !(MVar (Map (SomeTypeRep, Text) Dynamic)) -- ~ Dynamic ~ SomeTypeRep
    }

data AssetPathRef a = AssetPathRef
    { primary :: !(Weak (MVar a))
    , index   :: !Int
    }

data WeakAssetRefs = WeakAssetRefs
    { retrieve :: !(IO (Maybe Dynamic)) -- IO action to attempt to retrieve a if possible as a Dynamic
    , weak     :: !Dynamic -- ~ Weak a
    }

---------------
-- FilePaths --
---------------

data AssetIO m = AssetIO
    { loadFile    :: FilePath -> m (Maybe ByteString)
    , isValidFile :: FilePath -> m Bool
    , isValidPath :: FilePath -> m Bool
    }