{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Server (app) where

import CustomPrelude

import App (App (..), AppM)
import Control.Monad.Except (ExceptT (..))
import Data.UUID (UUID)
import qualified Handlers
import Lucid hiding (for_)
import OrphanInstances ()
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid
import WithPlayerApi (PlayerId (..))
import qualified WithPlayerApi

type API = Get '[HTML] (Html ()) :<|> StateChangeAPI :<|> "ws" :> WebSocket

type StateChangeAPI =
    Capture "stateId" UUID
        :> ( "join" :> Post '[HTML] (Html ())
                :<|> "leave"
                    :> ReqBody '[FormUrlEncoded] Handlers.LeavePost
                    :> Post '[HTML] (Html ())
                :<|> "settings"
                    :> ReqBody '[FormUrlEncoded] Handlers.SettingsPost
                    :> Post '[HTML] (Html ())
                :<|> "name"
                    :> ReqBody '[FormUrlEncoded] Handlers.NamePost
                    :> Post '[HTML] (Html ())
                :<|> "start" :> Post '[HTML] (Html ())
                :<|> "start-over" :> Post '[HTML] (Html ())
                :<|> "guess"
                    :> ReqBody '[FormUrlEncoded] Handlers.GuessPost
                    :> Post '[HTML] (Html ())
           )

api :: Proxy API
api = Proxy

stateChangeApi :: Proxy StateChangeAPI
stateChangeApi = Proxy

withPlayerApi :: Proxy (WithPlayerApi.API API)
withPlayerApi = Proxy

withPlayerApiServer :: App -> Maybe (Html ()) -> Server (WithPlayerApi.API API)
withPlayerApiServer a mHotReload =
    hoistServer
        withPlayerApi
        ( Servant.Handler . ExceptT . handleRIOServerErrors . fmap Right . runRIO a . logErrors
        )
        $ WithPlayerApi.withPlayerApi
            api
            (server mHotReload)
  where
    logErrors = handleAny $ \e -> logError (displayShow e) >> throwM e
    -- Lift thrown ServerErrors into Left
    handleRIOServerErrors = handle @IO @ServerError (pure . Left)

server :: Maybe (Html ()) -> PlayerId -> ServerT API AppM
server mHotReload playerId =
    Handlers.home api mHotReload playerId
        :<|> stateChangeServer playerId
        :<|> Handlers.ws api playerId

stateChangeServer :: PlayerId -> ServerT StateChangeAPI AppM
stateChangeServer playerId stateId =
    Handlers.join stateChangeApi playerId stateId
        :<|> Handlers.leave stateChangeApi playerId stateId
        :<|> Handlers.settings stateChangeApi playerId stateId
        :<|> Handlers.name stateChangeApi playerId stateId
        :<|> Handlers.start stateChangeApi playerId stateId
        :<|> Handlers.startOver stateChangeApi playerId stateId
        :<|> Handlers.guess stateChangeApi playerId stateId

app :: App -> Maybe (Html ()) -> Application
app a = serve withPlayerApi . withPlayerApiServer a
