{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Lib (app) where

import Control.Monad (replicateM_, unless, when)
import Data.Foldable (for_, sequenceA_, traverse_)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty.Zipper (Zipper)
import qualified Data.List.NonEmpty.Zipper as Z
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.UUID as UUID
import Game (GameState (..), Move (..), PlayerState (..), findInZipper, isGameOver, isPlayerAlive, isPlayerTurn, mkMove)
import Lucid hiding (for_)
import Servant
import Servant.HTML.Lucid
import WithPlayerApi (PlayerId (..))
import qualified WithPlayerApi

type API = Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

server :: Maybe (Html ()) -> Server (WithPlayerApi.API API)
server mHotReload = WithPlayerApi.withPlayerApi @API (allLinks api) $ home mHotReload

app :: Maybe (Html ()) -> Application
app = serve (Proxy :: (Proxy (WithPlayerApi.API API))) . server

home :: Maybe (Html ()) -> PlayerId -> Handler (Html ())
home mHotreload playerId = pure $ html_ $ do
    head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport_", content_ "width=device-width, initial-scale=1.0"]
        script_ [src_ "https://cdn.tailwindcss.com"] ("" :: String)
        script_ [src_ "https://unpkg.com/htmx.org@1.9.10", integrity_ "sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC", crossorigin_ "anonymous"] ("" :: String)
        title_ "Bombparty"
        sequenceA_ mHotreload
    body_ $ do
        h1_ $ toHtml $ UUID.toText $ getPlayerId playerId
        div_ [id_ "container", class_ "container mx-auto px-4"] $ do
            for_ [gs1, gs2, gs3, gs4] $ \gs -> do
                br_ []
                br_ []
                br_ []
                gameStateUI playerId gs

gameStateUI :: PlayerId -> GameState -> Html ()
gameStateUI pId gs = do
    when (isGameOver gs) $ h1_ "GAME OVER"
    unless (isGameOver gs) $ currentStringUI gs.currentString
    ul_ [class_ "space-y-3"] $ do
        traverse_ (playerStateUI gs) $ playerFirst pId gs.players

playerStateUI :: GameState -> PlayerState -> Html ()
playerStateUI gs ps = li_ [class_ $ "p-2 rounded-lg " <> bg <> " " <> outline] $ do
    h1_ $ toHtml $ UUID.toText $ getPlayerId ps.id
    letterUI ps.letters
    livesUI ps.lives
  where
    outline = if isPlayerTurn gs.players ps then "border-4 border-indigo-700" else "border-2"
    bg = if isPlayerAlive ps then "" else "bg-neutral-300"

livesUI :: Int -> Html ()
livesUI l = div_ $ replicateM_ l $ toHtml ("❤️" :: String)

currentStringUI :: Text -> Html ()
currentStringUI = div_ [class_ "text-2xl font-mono"] . toHtml

letterUI :: HashSet Char -> Html ()
letterUI ls = for_ ['a' .. 'z'] $ \l -> do
    let weight = if l `HashSet.member` ls then "font-extrabold" else "font-extralight"
    span_ [class_ $ "tracking-widest " <> weight] $ toHtml [l]

playerFirst :: PlayerId -> Zipper PlayerState -> [PlayerState]
playerFirst pId = flatten . findInZipper ((== pId) . (.id))
  where
    flatten :: Zipper a -> [a]
    flatten z = Z.current z : Z.rights z <> Z.lefts z

gs1 :: GameState
gs1 = GameState (Z.fromNonEmpty $ ps1 :| [ps2, ps3]) "THEQUICKBROWNFOXJUMPEDOVERTHELAZYDOG" mempty
gs2 :: GameState
gs2 = mkMove gs1 TimeUp
gs3 :: GameState
gs3 = mkMove gs2 TimeUp
gs4 :: GameState
gs4 = mkMove gs3 TimeUp

ps1 :: PlayerState
ps1 = PlayerState (PlayerId $ fromJust $ UUID.fromString "87c1c582-29a8-47bd-991d-ed07cc4f1e38") mempty 2
ps2 :: PlayerState
ps2 = PlayerState (PlayerId $ fromJust $ UUID.fromString "550e8400-e29b-41d4-a716-446655440000") mempty 2
ps3 :: PlayerState
ps3 = PlayerState (PlayerId $ fromJust $ UUID.fromString "112640ef-5e44-4be2-8379-346ab4c66acb") mempty 2
