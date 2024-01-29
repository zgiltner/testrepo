{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module App (AppM, App (..), Game) where

import RIO

import Game (GameState, Settings)
import Lucid (Html)

type AppM = RIO App

type Game = Either Settings GameState

data App = App
    { wsGameState :: TVar (Game, TChan (Either Game (Html ())))
    , wsGameStateTimer :: TVar (Maybe (Async ()))
    , logFunction :: LogFunc
    }

instance HasLogFunc App where
    logFuncL = lens (.logFunction) $ \a logFunction -> a{logFunction}
