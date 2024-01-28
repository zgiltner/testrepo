{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Handlers where

import RIO

import App (App (..), AppM)
import CaseInsensitive (CaseInsensitiveText)
import CircularZipper (CircularZipper (..))
import qualified CircularZipper as CZ
import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.HashSet as HashSet
import qualified Data.Text.Lazy as TL
import Game (
    GameState (..),
    Move (..),
    PlayerState (..),
    StartedGameState (..),
    UnStartedGameState (..),
    initialGameState,
    mkMove,
    startGame,
 )
import Lucid
import Lucid.Base (makeAttribute)
import Lucid.Htmx
import qualified Network.WebSockets as WS
import OrphanInstances ()
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid
import Timer (restartTimer, startTimer)
import Views (gameStateUI, guessInput, sharedHead)
import Web.FormUrlEncoded (FromForm (..))
import WithPlayerApi (PlayerId (..))

type APIConstraints api =
    ( IsElem ("leave" :> Post '[HTML] (Html ())) api
    , IsElem ("join" :> Post '[HTML] (Html ())) api
    , IsElem ("start" :> Post '[HTML] (Html ())) api
    , IsElem ("start-over" :> Post '[HTML] (Html ())) api
    , IsElem ("guess" :> Post '[HTML] (Html ())) api
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
    (gs, _) <- liftIO $ readTVarIO a.wsGameState
    pure $ html_ $ do
        head_ $ sharedHead mHotreload
        body_
            $ div_
                [ id_ "ws"
                , hxExt_ "ws"
                , makeAttribute "ws-connect" $ "/" <> toUrlPiece (safeLink api (Proxy @("ws" :> WebSocket)))
                , class_ "container mx-auto px-4 py-4"
                ]
            $ gameStateUI api me gs

updateGameState :: (GameState -> GameState) -> AppM GameState
updateGameState f = do
    a <- ask
    liftIO $ atomically $ do
        (gs, chan) <- readTVar a.wsGameState
        let gs' = f gs
        writeTChan chan $ Left gs'
        gs' <$ writeTVar a.wsGameState (gs', chan)

join ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    AppM (Html ())
join api me = do
    gs <- updateGameState
        $ \case
            GameStateUnStarted uGs -> GameStateUnStarted uGs{players = HashSet.insert me uGs.players}
            x -> x
    pure $ gameStateUI api me gs

leave ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    AppM (Html ())
leave api me = do
    gs <- updateGameState
        $ \case
            GameStateUnStarted uGs -> GameStateUnStarted uGs{players = HashSet.delete me uGs.players}
            x -> x
    pure $ gameStateUI api me gs

start ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    AppM (Html ())
start api me = do
    gs <- updateGameState $ \case
        GameStateUnStarted uGs -> startGame uGs
        x -> x
    a <- ask
    case gs of
        GameStateStarted _ -> liftIO $ startTimer a
        _ -> pure ()
    pure $ gameStateUI api me gs

startOver ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    AppM (Html ())
startOver api me = do
    gs <- updateGameState $ \case
        GameStateStarted gss ->
            startGame
                (initialGameState gss.stdGen gss.validWords gss.givenLettersSet)
                    { players = HashSet.fromList $ fmap (.id) $ toList $ getCircularZipper gss.players
                    }
        x -> x
    a <- ask
    case gs of
        GameStateStarted _ -> liftIO $ startTimer a
        _ -> pure ()
    pure $ gameStateUI api me gs

newtype GuessPost = GuessPost {guess :: CaseInsensitiveText}
    deriving stock (Show, Generic)

instance FromForm GuessPost

guess ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    GuessPost ->
    AppM (Html ())
guess api me p = do
    gs <- updateGameState $ \case
        GameStateStarted gs -> GameStateStarted $ mkMove gs $ Guess p.guess
        x -> x
    case gs of
        GameStateStarted gsS -> do
            a <- ask

            when ((CZ.current gsS.players).tries == 0) $ do
                -- It's the next players turn so let's restart the timer
                liftIO $ restartTimer a
        _ -> pure ()
    pure $ gameStateUI api me gs

newtype WsMsg = WsMsg
    { guess :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON)

ws ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    WS.Connection ->
    AppM ()
ws api me c = do
    a <- ask
    myChan <- liftIO $ atomically $ do
        (_, chan) <- readTVar a.wsGameState
        dupTChan chan
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
                    case eitherDecode @WsMsg msgString of
                        Left err -> logError $ "WebSocket received bad json: " <> fromString err
                        Right msg -> do
                            atomically $ writeTChan myChan $ Right $ guessInput msg.guess False False False me
                    listener
                _ -> listener
        sender :: AppM ()
        sender = do
            msg <- atomically $ readTChan myChan
            liftIO $ WS.sendTextData @Text c $ TL.toStrict $ renderText $ case msg of
                Left gs -> gameStateUI api me gs
                Right h -> h
            sender
    runConcurrently $ asum (Concurrently <$> [pingThread 0, listener, sender])
