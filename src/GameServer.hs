{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

module GameServer (app) where

import App (App (..), AppM)
import CaseInsensitive (CaseInsensitiveChar (..), CaseInsensitiveText)
import CircularZipper (CircularZipper (..))
import qualified CircularZipper as CZ
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, race_)
import Control.Concurrent.STM (atomically, dupTChan, readTChan, readTVar, readTVarIO, writeTChan, writeTVar)
import Control.Monad (replicateM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), ask)
import Data.Aeson (FromJSON, decode)
import Data.Foldable (for_, sequenceA_, toList, traverse_)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Game (
    GameState (..),
    Move (..),
    PlayerState (..),
    StartedGameState (..),
    UnStartedGameState (..),
    initialGameState,
    isGameOver,
    isPlayerAlive,
    isPlayerTurn,
    mkMove,
    startGame,
 )
import Lucid hiding (for_)
import Lucid.Base (makeAttribute)
import Lucid.Htmx
import qualified Network.WebSockets as WS
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid
import Web.FormUrlEncoded (FromForm (..))
import WithPlayerApi (PlayerId (..))
import qualified WithPlayerApi

newtype GuessPost = GuessPost {guess :: CaseInsensitiveText}
    deriving stock (Show, Generic)

instance FromForm GuessPost

type API =
    Get '[HTML] (Html ())
        :<|> "join" :> Post '[HTML] (Html ())
        :<|> "leave" :> Post '[HTML] (Html ())
        :<|> "start" :> Post '[HTML] (Html ())
        :<|> "start-over" :> Post '[HTML] (Html ())
        :<|> "guess"
            :> ReqBody '[FormUrlEncoded] GuessPost
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

app :: App -> Maybe (Html ()) -> Application
app a = serve withPlayerApi . withPlayerApiServer a

server :: Maybe (Html ()) -> PlayerId -> ServerT API AppM
server mHotReload playerId =
    home mHotReload playerId
        :<|> join playerId
        :<|> leave playerId
        :<|> start playerId
        :<|> startOver playerId
        :<|> guess playerId
        :<|> ws playerId

sharedHead :: Maybe (Html ()) -> Html ()
sharedHead mHotreload = head_ $ do
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport_", content_ "width=device-width, initial-scale=1.0"]
    script_ [src_ "https://cdn.tailwindcss.com"] ("" :: String)
    script_
        [ src_ "https://unpkg.com/htmx.org@1.9.10"
        , integrity_ "sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC"
        , crossorigin_ "anonymous"
        ]
        ("" :: String)
    script_ [src_ "https://unpkg.com/hyperscript.org@0.9.12"] ("" :: String)
    script_ [src_ "https://unpkg.com/htmx.org/dist/ext/ws.js"] ("" :: String)
    title_ "BombParty"
    sequenceA_ mHotreload

join :: PlayerId -> AppM (Html ())
join playerId = do
    gs <- updateGameState $
        \case
            GameStateUnStarted uGs -> GameStateUnStarted uGs{players = HashSet.insert playerId uGs.players}
            x -> x
    pure $ gameStateContainerUI playerId gs

leave :: PlayerId -> AppM (Html ())
leave playerId = do
    gs <- updateGameState $
        \case
            GameStateUnStarted uGs -> GameStateUnStarted uGs{players = HashSet.delete playerId uGs.players}
            x -> x
    pure $ gameStateContainerUI playerId gs

start :: PlayerId -> AppM (Html ())
start playerId = do
    gs <- updateGameState $ \case
        GameStateUnStarted uGs -> startGame uGs
        x -> x
    a <- ask
    case gs of
        GameStateStarted _ -> liftIO $ startTimer a
        _ -> pure ()
    pure $ gameStateContainerUI playerId gs

startOver :: PlayerId -> AppM (Html ())
startOver playerId = do
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
    pure $ gameStateContainerUI playerId gs

startTimer :: App -> IO ()
startTimer a = do
    t <- liftIO $ async go
    liftIO $ atomically $ writeTVar a.wsGameStateTimer $ Just t
  where
    go = do
        threadDelay 4000000
        gs <- liftIO $ atomically $ do
            (gs, chan) <- readTVar a.wsGameState
            case gs of
                (GameStateStarted gss) -> do
                    let gs' = GameStateStarted $ mkMove gss TimeUp
                    writeTVar a.wsGameState (gs', chan)
                    writeTChan chan $ Left gs'
                    pure gs'
                x -> pure x
        let gameOver = case gs of
                GameStateStarted gss -> isGameOver gss
                _ -> True
        unless gameOver go

stopTimer :: App -> IO ()
stopTimer a = maybe (pure ()) cancel =<< readTVarIO a.wsGameStateTimer

guess :: PlayerId -> GuessPost -> AppM (Html ())
guess playerId p = do
    gs <- updateGameState $ \case
        GameStateStarted gs -> GameStateStarted $ mkMove gs $ Guess p.guess
        x -> x
    case gs of
        GameStateStarted gsS -> do
            a <- ask
            -- It's the next players turn
            when ((CZ.current gsS.players).tries == 0) $ liftIO (stopTimer a >> startTimer a)
        _ -> pure ()
    pure $ gameStateContainerUI playerId gs

updateGameState :: (GameState -> GameState) -> AppM GameState
updateGameState f = do
    a <- ask
    liftIO $ atomically $ do
        (gs, chan) <- readTVar a.wsGameState
        let gs' = f gs
        -- We always want to write the value set by the above to the channel
        -- It's okay if it is stale
        writeTChan chan $ Left gs'
        gs' <$ writeTVar a.wsGameState (gs', chan)

home :: Maybe (Html ()) -> PlayerId -> AppM (Html ())
home mHotreload playerId = do
    a <- ask
    (gs, _) <- liftIO $ readTVarIO a.wsGameState
    pure $ gameStateUI mHotreload playerId gs

gameStateUI :: Maybe (Html ()) -> PlayerId -> GameState -> Html ()
gameStateUI mHotreload playerId gs = html_ $ do
    head_ $ sharedHead mHotreload
    body_
        $ div_
            [ id_ "ws"
            , hxExt_ "ws"
            , makeAttribute "ws-connect" "/ws"
            , class_ "container mx-auto px-4 py-4"
            ]
        $ gameStateContainerUI playerId gs

gameStateContainerUI :: PlayerId -> GameState -> Html ()
gameStateContainerUI playerId gs = div_ [id_ "gameState"] $ do
    case gs of
        GameStateUnStarted uGs -> do
            if playerId `elem` uGs.players
                then
                    button_
                        [ type_ "button"
                        , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-red-500 text-white hover:bg-red-600 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                        , hxPost_ "/leave"
                        , hxTarget_ "#gameState"
                        ]
                        "Leave Game"
                else
                    button_
                        [ type_ "button"
                        , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-blue-600 text-white hover:bg-blue-700 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                        , hxPost_ "/join"
                        , hxTarget_ "#gameState"
                        ]
                        "Join Game"
            ul_ $ for_ uGs.players $ \(PlayerId i) -> li_ $ toHtml $ T.pack $ show i
            unless (null uGs.players) $
                button_
                    [ type_ "button"
                    , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-green-500 text-white hover:bg-green-600 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                    , hxPost_ "/start"
                    , hxTarget_ "#gameState"
                    ]
                    "Start Game"
        GameStateStarted sGs -> do
            when (isGameOver sGs) $ do
                button_
                    [ type_ "button"
                    , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-green-500 text-white hover:bg-green-600 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                    , hxPost_ "/start-over"
                    , hxTarget_ "#gameState"
                    ]
                    "Start A New Game"
            unless (isGameOver sGs) $ div_ [class_ "text-2xl font-mono"] $ toHtml sGs.givenLetters
            ul_ [class_ "space-y-3"] $ do
                traverse_ (playerStateUI "" playerId sGs) $ playerFirst playerId sGs.players

playerStateUI :: Text -> PlayerId -> StartedGameState -> PlayerState -> Html ()
playerStateUI v pId gs ps = do
    li_ [class_ $ "p-2 rounded-lg " <> bg <> " " <> outline]
        $ form_
            [ hxPost_ "/guess"
            , hxTarget_ "#gameState"
            ]
        $ do
            if isPlayerAlive ps
                then
                    if isGameOver gs
                        then h1_ [class_ "text-center text-2xl"] "✨✨✨✨✨WINNER✨✨✨✨✨"
                        else do
                            letterUI ps.letters
                            div_ $ replicateM_ ps.lives $ toHtml ("❤️" :: String)
                            guessInput "" (pId == ps.id) (isPlayerTurn gs.players ps) ps.id
                else h1_ [class_ "text-center text-2xl"] "☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️"
  where
    outline =
        if isPlayerTurn gs.players ps
            then if isGameOver gs then "bg-emerald-600" else "border-4 border-blue-700"
            else "border-2"
    bg =
        if isPlayerAlive ps
            then if isGameOver gs then "bg-green-600" else ""
            else "bg-red-600"

playerInputId :: Bool -> PlayerId -> Text
playerInputId isMe playerId = "input-" <> UUID.toText (getPlayerId playerId) <> "-" <> if isMe then "me" else "other"

guessInput :: Text -> Bool -> Bool -> PlayerId -> Html ()
guessInput v isMe isMyTurn playerId = do
    input_
        ( [ id_ $ playerInputId isMe playerId
          , name_ "guess"
          , class_ $
                "border-2 "
                    <> if isActivePlayersTurn
                        then ""
                        else "bg-neutral-200"
          , value_ v
          ]
            <> [disabled_ "" | not isActivePlayersTurn]
            <> [autofocus_ | isActivePlayersTurn]
            <> [makeAttribute "ws-send" "" | isActivePlayersTurn]
            <> [hxTrigger_ "keyup changed delay:200ms" | isActivePlayersTurn]
        )
  where
    isActivePlayersTurn = isMe && isMyTurn

letterUI :: HashSet CaseInsensitiveChar -> Html ()
letterUI ls = for_ [(CaseInsensitiveChar 'A') .. (CaseInsensitiveChar 'Z')] $ \l -> do
    let weight = if l `HashSet.member` ls then "font-extrabold" else "font-extralight"
    span_ [class_ $ "tracking-widest " <> weight] $ toHtml l

playerFirst :: PlayerId -> CircularZipper PlayerState -> [PlayerState]
playerFirst pId cz = CZ.current playerCurrent : CZ.rights playerCurrent <> CZ.lefts playerCurrent
  where
    playerCurrent = fromMaybe cz $ CZ.findRight ((== pId) . (.id)) cz

data Msg = Msg
    { guess :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON)

ws :: PlayerId -> WS.Connection -> AppM ()
ws playerId c = do
    a <- ask
    myChan <- liftIO $ atomically $ do
        (_, chan) <- readTVar a.wsGameState
        dupTChan chan
    liftIO $ WS.withPingThread c 30 (pure ()) $ do
        let
            listener =
                WS.receive c >>= \case
                    WS.ControlMessage (WS.Close _ _) -> do
                        pure ()
                    WS.DataMessage _ _ _ (WS.Text msgString _) -> do
                        let Just msg = decode @Msg msgString
                        atomically $ writeTChan myChan $ Right $ guessInput msg.guess False False playerId
                        listener
                    _ -> listener
            sender = do
                msg <- atomically $ readTChan myChan
                WS.sendTextData @Text c $ TL.toStrict $ renderText $ case msg of
                    Left gs -> gameStateContainerUI playerId gs
                    Right h -> h
                sender
        race_ listener sender
