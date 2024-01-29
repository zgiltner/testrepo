{-# LANGUAGE OverloadedRecordDot #-}

module Timer (startTimer, stopTimer, restartTimer) where

import RIO

import App (App (..))
import Game (
    GameState (..),
    Move (..),
    Settings (secondsToGuess),
    isGameOver,
    mkMove,
 )

startTimer :: (MonadUnliftIO m) => App -> m ()
startTimer a = do
    t <- async go
    atomically $ writeTVar a.wsGameStateTimer $ Just t
  where
    go = do
        readTVarIO a.wsGameState >>= \case
            (Right gs, _) -> threadDelay $ gs.settings.secondsToGuess * 1000000
            _ -> pure ()
        gs <- atomically $ do
            (gs, chan) <- readTVar a.wsGameState
            case gs of
                (Right gss) -> do
                    let gs' = Right $ mkMove gss TimeUp
                    writeTVar a.wsGameState (gs', chan)
                    writeTChan chan $ Left gs'
                    pure gs'
                x -> pure x
        unless (either (const False) isGameOver gs) go

stopTimer :: (MonadIO m) => App -> m ()
stopTimer a = maybe (pure ()) cancel =<< readTVarIO a.wsGameStateTimer

restartTimer :: (MonadUnliftIO m) => App -> m ()
restartTimer a = stopTimer a >> startTimer a
