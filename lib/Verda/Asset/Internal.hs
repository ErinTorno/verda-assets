{-# LANGUAGE UnboxedTuples #-}

module Verda.Asset.Internal where

import           Control.Concurrent          (ThreadId, forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception           (SomeException, catch)
import           Control.Monad.IO.Unlift     (MonadUnliftIO(..), askUnliftIO, UnliftIO(UnliftIO))
import           Control.Monad               (forever, forM_, foldM)
import           Control.Monad.IO.Class      (MonadIO(..))
import           Control.Monad.ST            (stToIO)
import           Data.Default                (Default(def))
import           Data.Dynamic                (toDyn, fromDyn, Dynamic)
import           Data.Functor                ((<&>), ($>), void)
import qualified Data.HashTable.ST.Basic     as HT
import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import qualified Data.Sequence               as Seq
import           Data.Sequence               ((|>), Seq(..))
import           Data.String                 (IsString)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.TypeMap.Dynamic.Alt    as TM
import qualified Data.Vector.Mutable         as MVec
import           Data.Vector.Generic.Mutable (PrimMonad)
import qualified Data.Vector.Generic.Mutable as GMVec
import           System.FilePath             (takeExtension)
import           GHC.Stack                   (HasCallStack)
import qualified System.FSNotify             as FSNotify
import           System.Mem.Weak
import           Type.Reflection             (Typeable, typeOf)
import           Verda.Asset.File
import           Verda.Asset.Types

expectBug :: IsString s => s
expectBug = "This is a bug and should never happen!"

defaultAssetBufferLen :: Int
defaultAssetBufferLen = 256

mkDefaultAssets :: forall m mio. (MonadIO m, MonadIO mio) => mio (Assets m)
mkDefaultAssets = liftIO do
    let assetIO    = fileAssetIO @m
        loaders    = TM.empty
        settings   = def
    aliases <- do
        byOriginal <- newMVar HM.empty
        byClone    <- newMVar HM.empty
        pure Aliases {..}
    queues        <- AssetQueues <$> newMVar Seq.empty <*> newMVar Seq.empty <*> newMVar Seq.empty
    nextIndex     <- newMVar 0
    refsVector    <- newMVar =<< MVec.new defaultAssetBufferLen
    metaVector    <- newMVar =<< MVec.new defaultAssetBufferLen
    statuses      <- newMVar =<< MVec.replicate defaultAssetBufferLen Unloaded
    weakRefs      <- newMVar =<< MVec.new defaultAssetBufferLen
    refsByPath    <- stToIO $ HT.newSized defaultAssetBufferLen
    pure Assets {..}

withLoader :: forall m a. Typeable a => AssetLoader m a -> Assets m -> Assets m
withLoader loader assets = assets {loaders = TM.alter @a alt assets.loaders }
    where extMap = HM.fromList [(x, loader) |  x <- HS.toList loader.extensions]
          alt Nothing   = Just extMap
          alt (Just hm) = Just $ HM.union extMap hm

-------------------------
-- Threaded processing --
-------------------------

forkAssetLoader :: (AssetReader m, MonadUnliftIO m) => m ThreadId
forkAssetLoader = do
    assets <- askAssets
    UnliftIO unliftIO <- askUnliftIO
    liftIO . forkIO . FSNotify.withManager $ \_fsManager -> unliftIO $ forever do
        toLoad <- liftIO $ swapMVar assets.queues.toLoad Seq.empty

        forM_ toLoad \awaiting -> liftIO $ forkIO do
            void (unliftIO $ loadPrimaryAsset awaiting)
                `catch` \(e :: SomeException) -> writeStatus awaiting.index (Failed $ T.pack $ show e) assets

        updateWODQueue
        attemptFinishAliases

        liftIO $ threadDelay assets.settings.threadDelayMS

-- | Load the requested file, run it through its loader, and update the appropriate queues if it isn't fully loaded
loadPrimaryAsset :: (AssetReader m, MonadIO m) => AssetToLoad m -> m ()
loadPrimaryAsset awaiting = do
    assets <- askAssets
    
    assets.assetIO.loadFile awaiting.file >>= \case
        Nothing -> do
            let status = Failed $ T.pack $ "File doesn't exist: " <> show awaiting.file
            writeStatus awaiting.index status assets
        Just bytes -> do
            awaiting.loader.load awaiting.file bytes >>= \case
                Left msg ->
                    writeStatus awaiting.index (Failed msg) assets
                Right ctx -> do
                    liftIO $ modifyMVar_ assets.metaVector \v ->
                        MVec.write v awaiting.index ctx.meta $> v
                    case ctx.primary of
                        DynPKAlias aliasIndex ->
                            initAlias awaiting aliasIndex ctx
                        DynPKAsset primary -> do
                            let ctx' = primary <$ ctx
                            attemptFinishAsset awaiting ctx' >>= \case
                                Loaded   -> pure ()
                                Failed _ -> pure ()
                                _        -> liftIO $ modifyMVar_ assets.queues.toFinishAssets \queue ->
                                    pure (queue |> (awaiting, ctx'))

-- | Checks for each asset that is waiting on its dependencies to be loaded, and finishes them if they exist
updateWODQueue :: (AssetReader m, MonadUnliftIO m) => m ()
updateWODQueue =
    let nextDep acc = \case
            Empty -> pure acc
            (pair@(awaiting, ctx) :<| as) -> do
                attemptFinishAsset awaiting ctx >>= \case
                    Loaded   -> nextDep acc as
                    Failed _ -> nextDep acc as
                    _        -> nextDep (acc |> pair) as
     in do assets   <- askAssets
           tfa <- liftIO $ takeMVar assets.queues.toFinishAssets
           nextDep Empty tfa >>= \tfa' -> liftIO $ putMVar assets.queues.toFinishAssets tfa'

dependencyStatus :: (AssetReader m, MonadIO m) => LoadContext a -> m AssetStatus
dependencyStatus ctx =
    if   HS.null ctx.waitingOn.unwrap
    then pure Loaded
    else summedStatus ctx.waitingOn <&> \case
             Loading -> AwaitingDependencies
             s       -> s 

-- | Tries to finish this asset provided it and all dependencies are loaded, and outputs the new summed status
attemptFinishAsset :: (AssetReader m, MonadIO m) => AssetToLoad m -> LoadContext Dynamic -> m AssetStatus
attemptFinishAsset awaiting ctx = do
    assets <- askAssets
    status <- dependencyStatus ctx
    writeStatus awaiting.index status assets
    case status of 
        Loaded -> liftIO (awaiting.accept ctx.primary)
        _      -> pure ()
    pure status

initAlias :: forall m a. (AssetReader m, MonadIO m) => AssetToLoad m -> Int -> LoadContext a -> m ()
initAlias awaiting aliasIndex ctx = do
    assets     <- askAssets
    -- this handle might also be an alias! let's flatten the mappings
    origIndex <- liftIO (readMVar assets.aliases.byClone) <&>
        fromMaybe aliasIndex . HM.lookup aliasIndex
    liftIO do
        modifyMVar_ assets.queues.toLoad \queue -> pure $
            queue |> awaiting

        refsVector <- readMVar assets.refsVector
        refs       <- MVec.read refsVector origIndex
        modifyMVar_ assets.aliases.byClone $
            pure . HM.insert awaiting.index origIndex
        modifyMVar_ assets.aliases.byOriginal $
            pure . HM.adjust (HS.insert awaiting.index) origIndex
        awaiting.acceptRefs refs
        modifyMVar_ assets.refsVector \v ->
            MVec.write v awaiting.index refs $> v
        
        modifyMVar_ assets.queues.toFinishAliases \queue -> pure $
            queue |> (awaiting, origIndex <$ ctx)

attemptFinishAliases :: forall m. (AssetReader m, MonadIO m) => m ()
attemptFinishAliases = do
    assets <- askAssets
    liftIO do
        let go acc Empty = pure acc
            go acc (pair@(awaiting, ctx) :<| as) = do
                let origIndex = ctx.primary
                readStatus origIndex assets >>= \case
                    Loaded   -> do
                        tryReadMVar assets.weakRefs >>= \case
                            Nothing -> go (acc |> pair) as
                            Just weakRefs -> do
                                MVec.readMaybe weakRefs origIndex >>= \case
                                    Nothing  ->
                                        let expect = Failed $ T.pack $ "Asset " <> show origIndex <> " was loaded but weakRefs eval'ed to nothing; " <> expectBug
                                         in writeStatus awaiting.index expect assets
                                    Just wr  -> do
                                        let expect = Failed $ T.pack $ "Weak var retrieval for " <> show origIndex <> " failed; " <> expectBug

                                        wr.retrieve >>= \case
                                            Just a  -> do
                                                awaiting.accept a
                                                writeStatus awaiting.index Loaded assets
                                            Nothing ->
                                                writeStatus awaiting.index expect assets
                                go acc as   
                    Failed msg -> do
                        writeStatus awaiting.index (Failed msg) assets
                        go acc as
                    _        -> go (acc |> pair) as
        queue <- takeMVar assets.queues.toFinishAliases
        putMVar assets.queues.toFinishAliases =<< go Empty queue

------------
-- Status --
------------

isComplete :: AssetStatus -> Bool
isComplete = \case
    Failed _ -> True
    Loaded   -> True
    _        -> False

-- | Returns the status of the asset associated with the handle
assetStatus :: (AssetReader m, MonadIO m) => Handle a -> m AssetStatus
assetStatus !handle = askAssets >>= readStatus handle.index

readStatus :: MonadIO mio => Int -> Assets m -> mio AssetStatus
readStatus !index !assets = liftIO do
    statuses <- readMVar assets.statuses
    MVec.read statuses index

writeStatus :: MonadIO mio => Int -> AssetStatus -> Assets m -> mio ()
writeStatus !idx !status !assets = liftIO do
    statuses <- takeMVar assets.statuses

    MVec.write statuses idx status
    byOriginal <- readMVar assets.aliases.byOriginal
    forM_ (HM.lookup idx byOriginal) \clones ->
        forM_ clones \i ->
            MVec.write statuses i status

    putMVar assets.statuses statuses

combineStatus :: AssetStatus -> AssetStatus -> AssetStatus
combineStatus !a !b = case (# a, b #) of
    (# Loaded,    Loaded    #) -> Loaded
    (# Failed ta, Failed tb #) -> Failed (ta <> "; " <> tb)
    (# Failed ta, _         #) -> Failed ta
    (# _,         Failed tb #) -> Failed tb
    (# Unloaded,  _         #) -> Unloaded
    (# AwaitingDependencies, _ #) -> AwaitingDependencies
    (# _, AwaitingDependencies #) -> AwaitingDependencies
    _ -> Loading


summedStatus :: (AssetReader m, MonadIO m) => HandleSet -> m AssetStatus
summedStatus(HandleSet handles) = do
    assets   <- askAssets
    liftIO do
        statuses <- readMVar assets.statuses
        (\f -> foldM f Loaded handles) \acc index -> do
            status <- MVec.read statuses index
            pure $ combineStatus acc status

------------
-- Handle --
------------

isLoaded :: MonadIO m => Handle a -> m Bool
isLoaded !h = liftIO case h.ref of
    StrongRef _  -> pure True
    WeakRef weak -> deRefWeak weak >>= \case
        Nothing   -> pure False
        Just mvar -> isEmptyMVar mvar

toStrong :: MonadIO m => Handle a -> m (Maybe (Handle a))
toStrong !h = liftIO case h.ref of
    StrongRef _  -> pure $ Just h
    WeakRef weak -> deRefWeak weak <&> fmap \mvar -> h {ref = StrongRef mvar}

toWeak :: MonadIO m => Handle a -> m (Handle a)
toWeak !h = liftIO $ case h.ref of
    StrongRef mvar -> do
        weak <- mkWeakMVar mvar (pure ())
        pure h {ref = WeakRef weak}
    WeakRef _      -> pure h

getAsset :: MonadIO m => Handle a -> m (Maybe a)
getAsset !handle =
    if   handle.index < 0
    then pure Nothing
    else liftIO $ case handle.ref of
            StrongRef mvar -> tryReadMVar mvar
            WeakRef weak   -> deRefWeak weak >>= \case
                Nothing   -> pure Nothing
                Just mvar -> tryReadMVar mvar

{- HLint ignore "Use typeRep" -} -- doesn't compile using suggestion as of ghc 9.4.4 :/
loadHandle :: forall m a.
    ( AssetReader m
    , HasCallStack
    , MonadIO m
    , Typeable (MVar a), Typeable a
    ) => FilePath -> m (Handle a)
loadHandle !file = do
    assets <- askAssets
    let mkAccept !mvar !dyn =
            let expect    = error $ "fromDyn " <> show dyn <> " failed on AssetToLoad.accept; " <> expectBug
                !val :: a = fromDyn dyn expect
             in tryPutMVar mvar val >>= \case
                    True  -> pure ()
                    False -> putStrLn "loadHandle awaiting.accept failed to put value into mvar" 
        mkAcceptRefs !index !refs = do
            let expect   = error $ "fromDyn failed for primary asset for " <> show refs.primary <> " (expected " <> show (typeOf (undefined :: Weak (MVar a))) <> "); " <> expectBug
                !primary = fromDyn refs.primary expect
            stToIO $ HT.mutate assets.refsByPath file \case
                Nothing -> (,()) . Just $ TM.insert @a AssetPathRef {..} TM.empty
                Just tm -> (,()) . Just $ TM.insert @a AssetPathRef {..} tm
        resetRefs index weak = do
            meta <- newMVar Map.empty
            let refs = AssetRefs
                    { primary = toDyn weak
                    , labeled = HM.empty
                    , ..
                    }
                pathRef = AssetPathRef
                    { primary = weak
                    , index
                    }
            stToIO $ HT.mutate assets.refsByPath file \case
                Nothing -> (,()) . Just $ TM.insert @a pathRef TM.empty
                Just tm -> (,()) . Just $ TM.insert @a pathRef tm
            modifyMVar_ assets.refsVector $ growWrite index refs

        extension = T.pack $ drop 1 $ takeExtension file
        queueToLoad index mvar = case TM.lookup @a assets.loaders of
            Just extMap -> case HM.lookup extension extMap of
                Just typedLoader -> do
                    let loader = DynAssetLoader
                            { extensions = typedLoader.extensions
                            , load       = \file bytes -> typedLoader.load file bytes <&> fmap \ctx ->
                                ctx $> case ctx.primary of
                                    PKAsset a -> DynPKAsset (toDyn a)
                                    PKAlias h -> DynPKAlias h.index
                            }
                        acceptRefs = mkAcceptRefs index
                    modifyMVar_ assets.queues.toLoad \queue -> pure $
                        queue |> AssetToLoad {accept = mkAccept mvar, ..}
                Nothing -> do
                    let status = Failed $ "No asset loader for type " <> T.pack (show (typeOf (undefined :: a))) <> ", file extension ." <> extension
                    modifyMVar_ assets.statuses \v -> MVec.write v index status $> v
            Nothing -> do
                let status = Failed $ T.pack $ "No asset loader for type " <> show (typeOf (undefined :: a))
                modifyMVar_ assets.statuses \v -> MVec.write v index status $> v 

    liftIO (stToIO $ HT.lookup assets.refsByPath file) >>= \tm -> case tm >>= TM.lookup @a of
        Nothing -> liftIO do -- load from scratch, no refs found
            mvar :: MVar a <- newEmptyMVar
            let ref   = StrongRef mvar
            weakRefs <- takeMVar assets.weakRefs
            index    <- modifyMVar assets.nextIndex \i ->
                pure (i + 1, i)
            modifyMVar_ assets.metaVector $ growWrite index noMeta
            modifyMVar_ assets.statuses   $ growWrite index Loading
            weak      <- mkWeakPtr mvar Nothing
            weakRefs' <- growIfNeeded index bufferGrowIncrement weakRefs
            MVec.write weakRefs' index WeakAssetRefs
                { retrieve = deRefWeak weak >>= \case
                    Nothing   -> pure Nothing
                    Just mvar -> tryReadMVar mvar <&> fmap toDyn
                , weak     = toDyn weak
                }
            putMVar assets.weakRefs weakRefs'

            queueToLoad index mvar
            resetRefs index weak
            pure Handle {..}
        Just refs -> liftIO do -- refs exist, let's see if the Weak pointer is valid
            let index  = refs.index
            mvar   <- newEmptyMVar
            let ref = StrongRef mvar
            deRefWeak refs.primary >>= \case
                Just mvar ->
                    pure Handle {ref = StrongRef mvar, ..}
                Nothing -> do
                    queueToLoad index mvar
                    resetRefs index refs.primary
                    pure Handle {..}

assetMeta :: forall m a. (AssetReader m, MonadIO m) => Handle a -> m MetaMap
assetMeta handle = do
    assets <- askAssets
    liftIO do
        metaVec <- readMVar assets.metaVector
        MVec.read metaVec handle.index 

metaLookup :: forall a. Typeable a => Text -> MetaMap -> Maybe a
metaLookup label tm = TM.lookup @a tm >>= HM.lookup label

-----------
-- Utils --
-----------

bufferGrowIncrement :: Int
bufferGrowIncrement = 32

growWrite :: (PrimMonad m, GMVec.MVector v a) => Int -> a -> v (GMVec.PrimState m) a -> m (v (GMVec.PrimState m) a)
growWrite !index !a !v = do
    v' <- growIfNeeded index bufferGrowIncrement v
    GMVec.write v' index a
    pure v'

growIfNeeded :: (PrimMonad m, GMVec.MVector v a) => Int -> Int -> v (GMVec.PrimState m) a -> m (v (GMVec.PrimState m) a)
growIfNeeded !len !increment !v
    | len >= curLen = GMVec.grow v (len + increment)
    | otherwise     = pure v
    where curLen = GMVec.length v