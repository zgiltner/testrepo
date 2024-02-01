{-# LANGUAGE OverloadedLabels #-}

module Timer (startTimer, stopTimer, restartTimer) where

import CustomPrelude

import App (App (..), Game (..), _InGame)
import Data.UUID.V4 (nextRandom)
import Game (
    GameState (..),
    Move (..),
    Settings (secondsToGuess),
    isGameOver,
    mkMove,
 )

foo = to

startTimer :: (MonadUnliftIO m) => App -> m ()
startTimer a = do
    t <- async go
    atomically $ writeTVar (a ^. #wsGameStateTimer) $ Just t
  where
    go = do
        let secondsToGuessL = _1 % _2 % _InGame % #settings % #secondsToGuess
        mSecondsToGuess <- preview secondsToGuessL <$> readTVarIO (a ^. #wsGameState)
        traverse_ (threadDelay . (* 1000000)) mSecondsToGuess

        nextStateId <- liftIO nextRandom
        gs <- atomically $ do
            ((_, gs), chan) <- readTVar $ a ^. #wsGameState
            case gs of
                (InGame gss) -> do
                    let gs' = InGame $ mkMove gss TimeUp
                    writeTVar (a ^. #wsGameState) ((nextStateId, gs'), chan)
                    writeTChan chan (nextStateId, Left gs')
                    pure gs'
                x -> pure x

        for_ (gs ^? _InGame % to isGameOver) $ const go

stopTimer :: (MonadIO m) => App -> m ()
stopTimer a = maybe (pure ()) cancel =<< readTVarIO (a ^. #wsGameStateTimer)

restartTimer :: (MonadUnliftIO m) => App -> m ()
restartTimer a = stopTimer a >> startTimer a
