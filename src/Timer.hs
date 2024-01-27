{-# LANGUAGE OverloadedRecordDot #-}

module Timer (startTimer, stopTimer, restartTimer) where

import App (App (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTChan, writeTVar)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Game (
    GameState (..),
    Move (..),
    gameStateMicrosecondsToGuess,
    isGameOver,
    mkMove,
 )

startTimer :: App -> IO ()
startTimer a = do
    t <- liftIO $ async go
    liftIO $ atomically $ writeTVar a.wsGameStateTimer $ Just t
  where
    go = do
        threadDelay . gameStateMicrosecondsToGuess . fst =<< readTVarIO a.wsGameState
        gs <- liftIO $ atomically $ do
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

stopTimer :: App -> IO ()
stopTimer a = maybe (pure ()) cancel =<< readTVarIO a.wsGameStateTimer

restartTimer :: App -> IO ()
restartTimer a = stopTimer a >> startTimer a
