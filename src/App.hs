{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
    , logFunction :: LogFunc
    }

instance HasLogFunc App where
    logFuncL = lens (.logFunction) $ \a logFunction -> a{logFunction}
