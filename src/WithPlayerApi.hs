{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module WithPlayerApi (PlayerId (..), API, api, loginApi, withPlayerApi) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Servant
import Servant.Auth.Server (ThrowAll, throwAll)
import Servant.HTML.Lucid
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies)

newtype PlayerId = PlayerId {getPlayerId :: UUID}
    deriving stock (Eq, Show)

type API a =
    Header "Cookie" Text
        :> (LoginAPI :<|> a)

type LoginAPI =
    "login"
        :> Verb
            GET
            302
            '[HTML]
            ( Headers
                '[ Header "Set-Cookie" SetCookie
                 , Header "Location" Text
                 ]
                NoContent
            )

api :: Proxy (API a)
api = Proxy

loginApi :: Proxy LoginAPI
loginApi = Proxy

withPlayerApi ::
    forall a m.
    ( MonadIO m
    , ThrowAll (ServerT a m)
    , MonadError ServerError m
    ) =>
    Link ->
    (PlayerId -> ServerT a m) ->
    ServerT (API a) m
withPlayerApi afterLogin a mCookies = login :<|> aServer
  where
    aServer = case mPlayerId of
        Nothing -> throwAll $ redirectErr $ allLinks loginApi
        Just playerId -> a playerId
    fixLocationHeader l = if T.null l then "/" else l
    fixLocationHeader' l = if BS.null l then "/" else l
    redirectErr l =
        err302
            { errHeaders =
                ("Location", fixLocationHeader' $ toHeader l) : errHeaders err302
            }
    playerCookieName = "X-PLAYER-ID"
    mPlayerId =
        fmap PlayerId . UUID.fromASCIIBytes
            =<< lookup playerCookieName . parseCookies . encodeUtf8
            =<< mCookies
    login = case mPlayerId of
        Nothing -> do
            newPlayerId <- liftIO nextRandom
            pure
                $ addHeader
                    defaultSetCookie
                        { setCookieName = playerCookieName
                        , setCookieValue = encodeUtf8 $ UUID.toText newPlayerId
                        }
                $ addHeader (fixLocationHeader $ toUrlPiece afterLogin) NoContent
        Just _ -> throwError $ redirectErr afterLogin
