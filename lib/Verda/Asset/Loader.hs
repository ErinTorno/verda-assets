module Verda.Asset.Loader where

import           Data.Default      (Default(def))
import qualified Data.HashSet      as HS
import           Verda.Asset.Types

justAsset :: a -> LoadSuccess a
justAsset a = LoadContext
    { primary   = PKAsset a
    , waitingOn = mempty
    , meta      = def
    }

justAlias :: Handle a -> LoadSuccess a
justAlias a = LoadContext
    { primary   = PKAlias a
    , waitingOn = mempty
    , meta      = def
    }

withHandle :: Handle a -> HandleSet -> HandleSet
withHandle h hs = HandleSet $ HS.insert h.index hs.unwrap