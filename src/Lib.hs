{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lib (app) where

import App (App, AppM)
import CaseInsensitive (CaseInsensitiveChar (..), CaseInsensitiveText)
import CircularZipper (CircularZipper (..))
import qualified CircularZipper as CZ
import Control.Monad (replicateM_, unless, when)
import Control.Monad.Reader (ReaderT (..))
import Data.Foldable (for_, sequenceA_, traverse_)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Game (GameState (..), Move (..), PlayerState (..), isGameOver, isPlayerAlive, isPlayerTurn, mkMove, tryGuess)
import Lucid hiding (for_)
import Lucid.Htmx
import Servant
import Servant.HTML.Lucid
import Web.FormUrlEncoded (FromForm (..))
import WithPlayerApi (PlayerId (..))
import qualified WithPlayerApi

newtype GuessPost = GuessPost
    { guess :: CaseInsensitiveText
    }
    deriving stock (Generic)
    deriving anyclass (FromForm)

type API =
    Get '[HTML] (Html ())
        :<|> "gs" :> Capture "i" Int :> Get '[HTML] (Html ())
        :<|> "guess" :> ReqBody '[FormUrlEncoded] GuessPost :> Post '[HTML] (Html ())
        :<|> "time-up" :> Post '[HTML] (Html ())

api :: Proxy API
api = Proxy

server :: App -> Maybe (Html ()) -> Server (WithPlayerApi.API API)
server app mHotReload =
    WithPlayerApi.withPlayerApi
        @API
        (allLinks (Proxy :: Proxy (Get '[HTML] (Html ()))))
        (hoistServer api (`runReaderT` app) . foo)
  where
    foo playerId =
        home mHotReload playerId (gs1, 0)
            :<|> ( \i ->
                    home mHotReload playerId (iterate (`mkMove` TimeUp) gs1 !! i, i)
                 )
            :<|> guess mHotReload playerId gs1
            :<|> home mHotReload playerId (gs1, 0)

app :: App -> Maybe (Html ()) -> Application
app app = serve (Proxy :: (Proxy (WithPlayerApi.API API))) . server app

sharedHead :: Maybe (Html ()) -> Html ()
sharedHead mHotreload = do
    head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport_", content_ "width=device-width, initial-scale=1.0"]
        script_ [src_ "https://cdn.tailwindcss.com"] ("" :: String)
        script_
            [ src_ "https://unpkg.com/htmx.org@1.9.10"
            , integrity_ "sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC"
            , crossorigin_ "anonymous"
            ]
            ("" :: String)
        title_ "Bombparty"
        sequenceA_ mHotreload

guess :: Maybe (Html ()) -> PlayerId -> GameState -> GuessPost -> AppM (Html ())
guess mHotreload playerId gs gp = do
    let mGs = tryGuess gs gp.guess
    pure $ html_ $ do
        sharedHead mHotreload
        body_ $
            div_ [id_ "gameContainer"] $
                do
                    h1_ $ toHtml $ UUID.toText $ getPlayerId playerId
                    div_
                        [ class_ "container mx-auto px-4"
                        ]
                        $ gameStateUI playerId
                        $ fromMaybe gs mGs

home :: Maybe (Html ()) -> PlayerId -> (GameState, Int) -> AppM (Html ())
home mHotreload playerId (gs, i) = pure $ html_ $ do
    sharedHead mHotreload
    body_
        $ div_
            ( [ id_ "gameContainer"
              , hxGet_ ("/gs/" <> T.pack (show $ i + 1))
              , hxSwap_ "outerHTML"
              ]
                <> [hxTrigger_ "load delay:10s" | not $ isGameOver gs]
            )
        $ do
            h1_ $ toHtml $ UUID.toText $ getPlayerId playerId
            div_
                [ class_ "container mx-auto px-4"
                ]
                $ gameStateUI playerId gs

gameStateUI :: PlayerId -> GameState -> Html ()
gameStateUI pId gs = do
    when (isGameOver gs) $ h1_ "GAME OVER"
    unless (isGameOver gs) $ div_ [class_ "text-2xl font-mono"] $ toHtml gs.currentString
    ul_ [class_ "space-y-3"] $ do
        traverse_ (playerStateUI pId gs) $ playerFirst pId gs.players

playerStateUI :: PlayerId -> GameState -> PlayerState -> Html ()
playerStateUI pId gs ps = li_ [class_ $ "p-2 rounded-lg " <> bg <> " " <> outline] $ do
    h1_ $ toHtml $ UUID.toText $ getPlayerId ps.id
    letterUI ps.letters
    div_ $ replicateM_ ps.lives $ toHtml ("❤️" :: String)
    when (isPlayerAlive ps) $
        if isGameOver gs
            then h1_ "WINNER"
            else when (isPlayerAlive ps) $ do
                input_
                    ( [ id_ $ "input-" <> UUID.toText (getPlayerId ps.id)
                      , name_ "guess"
                      , class_ $
                            "border-2 "
                                <> if isActivePlayersTurn
                                    then ""
                                    else "bg-neutral-200"
                      , hxPost_ "/guess"
                      , hxTarget_ "#gameContainer"
                      ]
                        <> [disabled_ "" | not isActivePlayersTurn]
                        <> [autofocus_ | isActivePlayersTurn]
                    )
  where
    isActivePlayersTurn = pId == ps.id && isPlayerTurn gs.players ps
    outline = if isPlayerTurn gs.players ps then "border-4 border-indigo-700" else "border-2"
    bg = if isPlayerAlive ps then "" else "bg-neutral-300"

letterUI :: HashSet CaseInsensitiveChar -> Html ()
letterUI ls = for_ [(CaseInsensitiveChar 'A') .. (CaseInsensitiveChar 'Z')] $ \l -> do
    let weight = if l `HashSet.member` ls then "font-extrabold" else "font-extralight"
    span_ [class_ $ "tracking-widest " <> weight] $ toHtml l

playerFirst :: PlayerId -> CircularZipper PlayerState -> [PlayerState]
playerFirst pId cz = CZ.current playerCurrent : CZ.rights playerCurrent <> CZ.lefts playerCurrent
  where
    playerCurrent = fromMaybe cz $ CZ.findRight ((== pId) . (.id)) cz

wordList :: HashSet CaseInsensitiveText
wordList = HashSet.fromList ["the", "quick", "brown", "fox", "friday"]

gs1 :: GameState
gs1 = GameState (CZ.fromNonEmpty $ ps1 :| [ps2, ps3]) "fri" mempty wordList

ps1 :: PlayerState
ps1 = PlayerState (PlayerId $ fromJust $ UUID.fromString "169f6a54-19da-440d-abe6-e2d80d3a9d5b") mempty 2
ps2 :: PlayerState
ps2 = PlayerState (PlayerId $ fromJust $ UUID.fromString "550e8400-e29b-41d4-a716-446655440000") mempty 1
ps3 :: PlayerState
ps3 = PlayerState (PlayerId $ fromJust $ UUID.fromString "112640ef-5e44-4be2-8379-346ab4c66acb") mempty 2
