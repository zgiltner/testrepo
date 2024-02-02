{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module App (AppM, App (..), Game (..), AppGameState (..), StateKey (..), _InGame, _InLobby) where

import CustomPrelude

import Game (GameState, Settings)
import Lucid (Html)
import Optics.TH (makePrisms)
import qualified RIO
import Servant (FromHttpApiData, ToHttpApiData)

data Game = InLobby Settings | InGame GameState
makePrisms ''Game

type AppM = RIO App

newtype StateKey = StateKey {getStateKey :: Int}
    deriving stock (Eq, Show)
    deriving newtype (Num, Display, FromHttpApiData, ToHttpApiData)

data AppGameState = AppGameState
    { stateKey :: StateKey
    , game :: Game
    , chan :: TChan (StateKey, Either Game (Html ()))
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
