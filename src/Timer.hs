{-# LANGUAGE OverloadedRecordDot #-}

module Timer (startTimer, stopTimer, restartTimer) where

import RIO

import App (App (..))
import Game (
    GameState (..),
    Move (..),
    gameStateMicrosecondsToGuess,
    isGameOver,
    mkMove,
 )

startTimer :: (MonadUnliftIO m) => App -> m ()
startTimer a = do
    t <- async go
    atomically $ writeTVar a.wsGameStateTimer $ Just t
  where
    go = do
        threadDelay . gameStateMicrosecondsToGuess . fst =<< readTVarIO a.wsGameState
        gs <- atomically $ do
            (gs, chan) <- readTVar a.wsGameState
            case gs of
                (GameStateStarted gss) -> do
                    let gs' = GameStateStarted $ mkMove gss TimeUp
                    writeTVar a.wsGameState (gs', chan)
                    writeTChan chan $ Left gs'
                    pure gs'
                x -> pure x
        let gameOver = case gs of
                GameStateStarted gss -> isGameOver gss
                _ -> True
        unless gameOver go

stopTimer :: (MonadIO m) => App -> m ()
stopTimer a = maybe (pure ()) cancel =<< readTVarIO a.wsGameStateTimer

restartTimer :: (MonadUnliftIO m) => App -> m ()
restartTimer a = stopTimer a >> startTimer a
