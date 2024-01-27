{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module App (AppM, App (..)) where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TChan, TVar)
import Game (GameState)
import Lucid (Html)
import RIO (RIO)

type AppM = RIO App

data App = App
    { wsGameState :: TVar (GameState, TChan (Either GameState (Html ())))
    , wsGameStateTimer :: TVar (Maybe (Async ()))
    }
