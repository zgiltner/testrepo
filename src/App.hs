{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}

module App (AppM, App (..), Game, GameWithStateId) where

import CustomPrelude

import Data.UUID (UUID)
import Game (GameState, Settings)
import Lucid (Html)
import qualified RIO

type AppM = RIO App

type GameWithStateId = (UUID, Game)
type Game = Either Settings GameState

data App = App
    { wsGameState :: TVar (GameWithStateId, TChan (UUID, Either Game (Html ())))
    , wsGameStateTimer :: TVar (Maybe (Async ()))
    , logFunction :: LogFunc
    , staticDir :: FilePath
    }
    deriving (Generic)

instance HasLogFunc App where
    logFuncL = RIO.lens (view #logFunction) (flip $ set #logFunction)
