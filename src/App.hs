{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module App (AppM, App (..)) where

import RIO

import Game (GameState)
import Lucid (Html)

type AppM = RIO App

data App = App
    { wsGameState :: TVar (GameState, TChan (Either GameState (Html ())))
    , wsGameStateTimer :: TVar (Maybe (Async ()))
    }

instance HasLogFunc App
