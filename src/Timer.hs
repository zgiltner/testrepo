{-# LANGUAGE OverloadedLabels #-}

module Timer (startTimer, stopTimer, restartTimer) where

import CustomPrelude

import App (App (..), AppGameState (..), Game (..), _InGame)
import Data.UUID.V4 (nextRandom)
import Game (
    GameState (..),
    Move (..),
    Settings (secondsToGuess),
    isGameOver,
    makeMove,
 )

startTimer :: (MonadUnliftIO m) => App -> m ()
startTimer a = do
    t <- async go
    atomically $ writeTVar (a ^. #wsGameStateTimer) $ Just t
  where
    go = do
        let secondsToGuessL = #game % _InGame % #settings % #secondsToGuess
        mSecondsToGuess <- preview secondsToGuessL <$> readTVarIO (a ^. #wsGameState)
        traverse_ (threadDelay . (* 1000000)) mSecondsToGuess

        gs <- atomically $ do
            appGameState <- readTVar $ a ^. #wsGameState
            let nextStateKey = 1 + appGameState ^. #stateKey
            case appGameState ^. #game of
                (InGame gss) -> do
                    let gs' = InGame $ makeMove gss TimeUp
                    writeTVar (a ^. #wsGameState) $ appGameState{game = gs', stateKey = nextStateKey}
                    writeTChan (appGameState ^. #chan) (nextStateKey, Left gs')
                    pure gs'
                x -> pure x

        case gs ^? _InGame % to isGameOver of
            Just True -> pure ()
            _ -> go

stopTimer :: (MonadIO m) => App -> m ()
stopTimer a = maybe (pure ()) cancel =<< readTVarIO (a ^. #wsGameStateTimer)

restartTimer :: (MonadUnliftIO m) => App -> m ()
restartTimer a = stopTimer a >> startTimer a
