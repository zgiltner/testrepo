{-# LANGUAGE OverloadedLabels #-}

module Timer (startTimer, stopTimer, restartTimer) where

import CustomPrelude

import App (App (..), AppGameState (..), Game (..), _InGame)
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

        join $ atomically $ do
            appGameState <- readTVar $ a ^. #wsGameState
            case appGameState ^. #game of
                (InGame gss) -> do
                    let gss' = makeMove gss TimeUp
                        gs' = InGame gss'
                        nextStateKey = 1 + appGameState ^. #stateKey
                        appGameState' =
                            appGameState
                                & (#game .~ gs')
                                & (#stateKey .~ nextStateKey)

                    writeTVar (a ^. #wsGameState) appGameState'
                    writeTChan (appGameState ^. #chan) (nextStateKey, Left gs')
                    pure $ unless (isGameOver gss') go
                _ -> pure $ pure ()

stopTimer :: (MonadIO m) => App -> m ()
stopTimer a = maybe (pure ()) cancel =<< readTVarIO (a ^. #wsGameStateTimer)

restartTimer :: (MonadUnliftIO m) => App -> m ()
restartTimer a = stopTimer a >> startTimer a
