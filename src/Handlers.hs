{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Handlers where

import CustomPrelude

import App (App (..), AppGameState (..), AppM, Game (..), _InGame)
import CaseInsensitive (CaseInsensitiveText)
import qualified CircularZipper as CZ
import qualified Data.Aeson as Aeson
import Data.Aeson.QQ (aesonQQ)
import qualified Data.Text.Lazy as TL
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Game (
    GameState (..),
    Move (..),
    PlayerState (..),
    Settings (..),
    isGameOver,
    makeMove,
    startGame,
 )
import GameStateEvent (GameStateEvent (..), getGameStateEvents)
import Lucid hiding (for_)
import Lucid.Base (makeAttribute)
import Lucid.Htmx
import qualified Network.WebSockets as WS
import OrphanInstances ()
import qualified RIO.ByteString.Lazy as BSL
import qualified RIO.HashMap as HashMap
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid
import Timer (restartTimer, startTimer, stopTimer)
import Views (gameStateUI, guessInput, sharedHead)
import Web.FormUrlEncoded (FromForm (..))
import WithPlayerApi (PlayerId (..))

type APIConstraints api =
    ( IsElem
        ( Capture "stateId" UUID
            :> "leave"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "join"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "start"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "settings"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "name"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "start-over"
            :> Post '[HTML] (Headers '[Header "HX-Trigger-After-Swap" GameStateEvent] (Html ()))
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "guess"
            :> Post '[HTML] (Headers '[Header "HX-Trigger-After-Swap" GameStateEvent] (Html ()))
        )
        api
    )

home ::
    ( APIConstraints api
    , IsElem ("ws" :> WebSocket) api
    ) =>
    Proxy api ->
    Maybe (Html ()) ->
    PlayerId ->
    AppM (Html ())
home api mHotreload me = do
    a <- ask
    appGameState <- liftIO $ readTVarIO $ a ^. #wsGameState
    pure $ html_ $ do
        head_ $ sharedHead mHotreload
        body_
            $ div_
                [ id_ "ws"
                , hxExt_ "ws,transform-ws-response"
                , makeAttribute "ws-connect" $ "/" <> toUrlPiece (safeLink api (Proxy @("ws" :> WebSocket)))
                , class_ "container mx-auto px-4 py-4"
                ]
            $ gameStateUI api me (appGameState ^. #gameStateId) (appGameState ^. #game)

updateGameState :: UUID -> (Game -> Game) -> AppM Game
updateGameState gameStateId f = do
    a <- ask
    liftIO $ do
        stateId' <- nextRandom
        atomically $ do
            appGameState <- readTVar $ a ^. #wsGameState
            if (appGameState ^. #gameStateId) == gameStateId
                then do
                    let gs' = f $ appGameState ^. #game
                    writeTChan (appGameState ^. #chan) (stateId', Left gs')
                    gs' <$ writeTVar (a ^. #wsGameState) appGameState{gameStateId = stateId', game = gs'}
                else pure $ appGameState ^. #game

join ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    UUID ->
    AppM (Html ())
join api me stateId = do
    gs <- updateGameState stateId
        $ \case
            InLobby settings -> InLobby $ settings & #players %~ HashMap.insert me Nothing
            x -> x
    pure $ gameStateUI api me stateId gs

newtype LeavePost = LeavePost
    {playerId :: PlayerId}
    deriving stock (Show, Generic)

instance FromForm LeavePost

leave ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    UUID ->
    LeavePost ->
    AppM (Html ())
leave api me stateId p = do
    gs <- updateGameState stateId
        $ \case
            InLobby settings -> InLobby $ settings & #players %~ HashMap.delete (p ^. #playerId)
            x -> x
    pure $ gameStateUI api me stateId gs

data SettingsPost = SettingsPost
    {secondsToGuess :: Int}
    deriving stock (Show, Generic)

instance FromForm SettingsPost

settings ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    UUID ->
    SettingsPost ->
    AppM (Html ())
settings api me stateId p = do
    gs <- updateGameState stateId
        $ \case
            InLobby settings -> InLobby $ settings & #secondsToGuess .~ p ^. #secondsToGuess
            x -> x
    pure $ gameStateUI api me stateId gs

data NamePost = NamePost
    {playerId :: PlayerId, name :: Text}
    deriving stock (Show, Generic)

instance FromForm NamePost

name ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    UUID ->
    NamePost ->
    AppM (Html ())
name api me stateId p = do
    gs <- updateGameState stateId
        $ \case
            InLobby settings -> InLobby $ settings & #players %~ HashMap.update (const $ Just $ Just $ p ^. #name) (p ^. #playerId)
            x -> x
    pure $ gameStateUI api me stateId gs

start ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    UUID ->
    AppM (Html ())
start api me stateId = do
    gs <- updateGameState stateId $ \case
        InLobby settings -> maybe (InLobby settings) InGame $ startGame settings
        x -> x
    a <- ask
    case gs of
        InGame _ -> startTimer a
        _ -> pure ()
    pure $ gameStateUI api me stateId gs

startOver ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    UUID ->
    AppM (Headers '[Header "HX-Trigger-After-Swap" GameStateEvent] (Html ()))
startOver api me stateId = do
    stopTimer =<< ask
    gs <- updateGameState stateId $ \case
        InGame gs ->
            InLobby $ gs ^. #settings
        x -> x
    pure $ addHeader GameOver $ gameStateUI api me stateId gs

newtype GuessPost = GuessPost {guess :: CaseInsensitiveText}
    deriving stock (Show, Generic)

instance FromForm GuessPost

guess ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    UUID ->
    GuessPost ->
    AppM (Headers '[Header "HX-Trigger-After-Swap" GameStateEvent] (Html ()))
guess api me stateId p = do
    gs <- updateGameState stateId $ \case
        InGame gs -> InGame $ makeMove gs $ Guess $ p ^. #guess
        x -> x
    let html = gameStateUI api me stateId gs
    case gs of
        InGame gsS -> do
            a <- ask

            -- It's the next players turn
            if CZ.current (gsS ^. #players) ^. #tries == 0
                then
                    if isGameOver gsS
                        then do
                            stopTimer a
                            pure $ addHeader GameOver html
                        else do
                            restartTimer a
                            pure $ addHeader CorrectGuess html
                else pure $ addHeader WrongGuess html
        _ -> pure $ noHeader html

newtype WsMsg = WsMsg
    { guess :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (Aeson.FromJSON)

ws ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    WS.Connection ->
    AppM ()
ws api me c = do
    a <- ask
    myChan <- atomically $ dupTChan . view #chan =<< readTVar (a ^. #wsGameState)
    let
        pingThread :: Int -> AppM ()
        pingThread i = do
            threadDelay 30000000
            liftIO $ WS.sendPing c $ tshow i
            pingThread $ i + 1
        listener :: AppM ()
        listener =
            liftIO (WS.receive c) >>= \case
                WS.ControlMessage (WS.Close _ _) -> pure ()
                WS.DataMessage _ _ _ (WS.Text msgString _) -> do
                    case Aeson.eitherDecode @WsMsg msgString of
                        Left err -> logError $ "WebSocket received bad json: " <> fromString err
                        Right msg -> do
                            atomically $ do
                                appGameState <- readTVar $ a ^. #wsGameState
                                writeTChan myChan (appGameState ^. #gameStateId, Right $ guessInput (msg ^. #guess) False False False me)
                    listener
                _ -> listener
        sender :: AppM ()
        sender = do
            mMsg <- atomically $ do
                (stateId, msg) <- readTChan myChan
                appGameState <- readTVar $ a ^. #wsGameState
                pure $ if stateId == (appGameState ^. #gameStateId) then Just (stateId, msg) else Nothing

            liftIO $ for_ mMsg $ \msg -> WS.sendTextData @Text c $ case msg of
                (stateId, Left gs) ->
                    decodeUtf8Lenient
                        $ BSL.toStrict
                        $ Aeson.encode [aesonQQ|{html: #{renderText $ gameStateUI api me stateId gs}, events: #{getGameStateEvents me gs}}|]
                (_, Right h) -> TL.toStrict $ renderText h
            sender
    runConcurrently $ asum (Concurrently <$> [pingThread 0, listener, sender])
