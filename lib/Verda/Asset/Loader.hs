module Verda.Asset.Loader where

import           Data.Text                    (Text)
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import qualified Data.TypeMap.Dynamic.Alt     as TM
import           Type.Reflection              (Typeable)
import           Verda.Asset.Types

justAsset :: a -> LoadSuccess a
justAsset a = LoadContext
    { primary   = PKAsset a
    , waitingOn = mempty
    , meta      = TM.empty
    }

justAlias :: Handle a -> LoadSuccess a
justAlias a = LoadContext
    { primary   = PKAlias a
    , waitingOn = mempty
    , meta      = TM.empty
    }

withMeta :: forall mt. Typeable mt => Text -> mt -> MetaMap -> MetaMap
withMeta label m = TM.alter @mt alt
    where alt (Just hm) = Just $ HM.insert label m hm
          alt Nothing   = Just $ HM.singleton label m

withHandle :: Handle a -> HandleSet -> HandleSet
withHandle h hs = HandleSet $ HS.insert h.index hs.unwrap