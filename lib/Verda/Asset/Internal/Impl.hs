{-# LANGUAGE UndecidableInstances #-}

module Verda.Asset.Internal.Impl where

-- | A sketchy typeclass hack to allow polymorphic HasField instances
class Impl a | -> a
instance Impl a => Impl a