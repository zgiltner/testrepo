{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Server (app) where

import App (App (..), AppM)
import Control.Monad.Reader (ReaderT (..))
import qualified Handlers
import Lucid hiding (for_)
import OrphanInstances ()
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid
import WithPlayerApi (PlayerId (..))
import qualified WithPlayerApi

type API =
    Get '[HTML] (Html ())
        :<|> "join" :> Post '[HTML] (Html ())
        :<|> "leave" :> Post '[HTML] (Html ())
        :<|> "start" :> Post '[HTML] (Html ())
        :<|> "start-over" :> Post '[HTML] (Html ())
        :<|> "guess"
            :> ReqBody '[FormUrlEncoded] Handlers.GuessPost
            :> Post '[HTML] (Html ())
        :<|> "ws" :> WebSocket

api :: Proxy API
api = Proxy

withPlayerApi :: Proxy (WithPlayerApi.API API)
withPlayerApi = Proxy

withPlayerApiServer :: App -> Maybe (Html ()) -> Server (WithPlayerApi.API API)
withPlayerApiServer a mHotReload =
    hoistServer withPlayerApi (`runReaderT` a) $
        WithPlayerApi.withPlayerApi
            api
            (server mHotReload)

server :: Maybe (Html ()) -> PlayerId -> ServerT API AppM
server mHotReload playerId =
    Handlers.home api mHotReload playerId
        :<|> Handlers.join api playerId
        :<|> Handlers.leave api playerId
        :<|> Handlers.start api playerId
        :<|> Handlers.startOver api playerId
        :<|> Handlers.guess api playerId
        :<|> Handlers.ws api playerId

app :: App -> Maybe (Html ()) -> Application
app a = serve withPlayerApi . withPlayerApiServer a
