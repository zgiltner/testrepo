{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module App (AppM, App (..)) where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TChan, TVar)
import Control.Monad.Reader (ReaderT)
import Game (GameState)
import Lucid (Html)
import Servant (Handler)

type AppM = ReaderT App Handler

data App = App
    { wsGameState :: TVar (GameState, TChan (Either GameState (Html ())))
    , wsGameStateTimer :: TVar (Maybe (Async ()))
    }
