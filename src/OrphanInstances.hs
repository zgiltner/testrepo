{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances () where

import Servant (HasLink (..))
import Servant.API.WebSocket (WebSocket)

instance HasLink WebSocket where
    type MkLink WebSocket a = a
    toLink toA _ = toA
