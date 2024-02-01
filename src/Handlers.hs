{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Handlers where

import CustomPrelude

import App (App (..), AppM, Game)
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
    mkMove,
    startGame,
 )
import Lucid
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
            :> Post '[HTML] (Headers '[Header "HX-Trigger-After-Swap" Text] (Html ()))
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "guess"
            :> Post '[HTML] (Headers '[Header "HX-Trigger-After-Swap" Text] (Html ()))
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
    ((stateId, gs), _) <- liftIO $ readTVarIO $ a ^. #wsGameState
    pure $ html_ $ do
        head_ $ sharedHead mHotreload
        body_
            $ div_
                [ id_ "ws"
                , hxExt_ "ws,transform-ws-response"
                , makeAttribute "ws-connect" $ "/" <> toUrlPiece (safeLink api (Proxy @("ws" :> WebSocket)))
                , class_ "container mx-auto px-4 py-4"
                ]
            $ gameStateUI api me stateId gs

updateGameState :: UUID -> (Game -> Game) -> AppM Game
updateGameState stateId f = do
    a <- ask
    liftIO $ do
        stateId' <- nextRandom
        atomically $ do
            ((currStateId, gs), chan) <- readTVar $ a ^. #wsGameState
            if currStateId == stateId
                then do
                    let gs' = f gs
                    writeTChan chan (stateId', Left gs')
                    gs' <$ writeTVar (a ^. #wsGameState) ((stateId', gs'), chan)
                else pure gs

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
            Left settings -> Left $ settings & #players %~ HashMap.insert me Nothing
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
            Left settings -> Left $ settings & #players %~ HashMap.delete (p ^. #playerId)
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
            Left settings -> Left $ settings & #secondsToGuess .~ p ^. #secondsToGuess
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
            Left settings -> Left $ settings & #players %~ HashMap.update (const $ Just $ Just $ p ^. #name) (p ^. #playerId)
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
        Left settings -> maybe (Left settings) Right $ startGame settings
        x -> x
    a <- ask
    case gs of
        Right _ -> startTimer a
        _ -> pure ()
    pure $ gameStateUI api me stateId gs

startOver ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    UUID ->
    AppM (Headers '[Header "HX-Trigger-After-Swap" Text] (Html ()))
startOver api me stateId = do
    stopTimer =<< ask
    gs <- updateGameState stateId $ \case
        Right gs ->
            Left $ gs ^. #settings
        x -> x
    pure $ addHeader "gameOver" $ gameStateUI api me stateId gs

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
    AppM (Headers '[Header "HX-Trigger-After-Swap" Text] (Html ()))
guess api me stateId p = do
    gs <- updateGameState stateId $ \case
        Right gs -> Right $ mkMove gs $ Guess $ p ^. #guess
        x -> x
    mSound <- case gs of
        Right gsS -> do
            a <- ask

            -- It's the next players turn
            if CZ.current (gsS ^. #players) ^. #tries == 0
                then
                    if isGameOver gsS
                        then do
                            stopTimer a
                            pure $ Just "gameOver"
                        else do
                            restartTimer a
                            pure $ Just "correctGuess"
                else pure $ Just "wrongGuess"
        _ -> pure mempty
    pure $ addHeader (fromMaybe "" mSound) $ gameStateUI api me stateId gs

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
    myChan <- atomically $ do
        (_, chan) <- readTVar $ a ^. #wsGameState
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
                    case Aeson.eitherDecode @WsMsg msgString of
                        Left err -> logError $ "WebSocket received bad json: " <> fromString err
                        Right msg -> do
                            atomically $ do
                                ((currStateId, _), _) <- readTVar $ a ^. #wsGameState
                                writeTChan myChan (currStateId, Right $ guessInput (msg ^. #guess) False False False me)
                    listener
                _ -> listener
        sender :: AppM ()
        sender = do
            mMsg <- atomically $ do
                (stateId, msg) <- readTChan myChan
                ((currStateId, _), _) <- readTVar $ a ^. #wsGameState
                pure $ if stateId == currStateId then Just (stateId, msg) else Nothing

            liftIO $ case mMsg of
                Just msg -> WS.sendTextData @Text c $ case msg of
                    (stateId, Left gs) ->
                        let
                            html = gameStateUI api me stateId gs
                            mEvents :: Maybe [Text] = do
                                let gameOver = maybe (Just ["gameOver"]) (bool Nothing (Just ["gameOver"]) . isGameOver) $ gs ^? _Right
                                gameOver <> do
                                    players <- gs ^? _Right % #players
                                    isFirstRound <- (== 0) <$> gs ^? _Right % #round
                                    let
                                        currentPlayer = CZ.current players
                                        lastPlayer = CZ.current $ CZ.left players
                                        turnStarting = currentPlayer ^. #tries == 0
                                        wasMyTurn = lastPlayer ^. #id == me
                                        lastPlayerCorrect = isJust $ lastPlayer ^. #lastWord
                                        isMyTurn = turnStarting && currentPlayer ^. #id == me
                                        iWin = do
                                            gOver <- isGameOver <$> gs ^? _Right
                                            if gOver && isMyTurn then Just ["iWin"] else Nothing
                                        myTurn = bool Nothing (Just ["myTurn"]) isMyTurn
                                        timeUp = bool Nothing (Just ["timeUp"]) $ turnStarting && wasMyTurn && not lastPlayerCorrect && not isFirstRound
                                    iWin <|> myTurn <|> timeUp
                         in
                            decodeUtf8Lenient $ BSL.toStrict $ Aeson.encode [aesonQQ|{html: #{renderText html }, events: #{mEvents}}|]
                    (_, Right h) -> TL.toStrict $ renderText h
                Nothing -> pure ()
            sender
    runConcurrently $ asum (Concurrently <$> [pingThread 0, listener, sender])
