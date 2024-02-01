{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module App (AppM, App (..), Game (..), AppGameState (..), _InGame, _InLobby) where

import CustomPrelude

import qualified CircularZipper as CZ
import Data.UUID (UUID)
import Game (GameState, Move, Settings, isGameOver)
import qualified Game
import Lucid (Html)
import Optics.TH (makePrisms)
import qualified RIO
import WithPlayerApi (PlayerId)

data Game = InLobby Settings | InGame GameState
makePrisms ''Game

type AppM = RIO App

data AppGameState = AppGameState
    { gameStateId :: UUID
    , game :: Game
    , chan :: TChan (UUID, Either Game (Html ()))
    }
    deriving (Generic)

data App = App
    { wsGameState :: TVar AppGameState
    , wsGameStateTimer :: TVar (Maybe (Async ()))
    , logFunction :: LogFunc
    , staticDir :: FilePath
    }
    deriving (Generic)

instance HasLogFunc App where
    logFuncL = RIO.lens (view #logFunction) (flip $ set #logFunction)
