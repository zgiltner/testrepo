{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module App (AppM, App (..), Game (..), _InGame, _InLobby, GameWithStateId) where

import CustomPrelude

import qualified CircularZipper as CZ
import Data.UUID (UUID)
import Game (GameState, Move, Settings, isGameOver)
import qualified Game
import Lucid (Html)
import Optics.TH (makePrisms)
import qualified RIO
import WithPlayerApi (PlayerId)

type GameWithStateId = (UUID, Game)

data Game = InLobby Settings | InGame GameState
makePrisms ''Game

type AppM = RIO App
data App = App
    { wsGameState :: TVar (GameWithStateId, TChan (UUID, Either Game (Html ())))
    , wsGameStateTimer :: TVar (Maybe (Async ()))
    , logFunction :: LogFunc
    , staticDir :: FilePath
    }
    deriving (Generic)

instance HasLogFunc App where
    logFuncL = RIO.lens (view #logFunction) (flip $ set #logFunction)

data GameEvent = TimeUp | CorrectGuess | MyTurn | GameOver | IWin

makeMove :: PlayerId -> GameState -> Move -> (Maybe [GameEvent], GameState)
makeMove me gs move = (gameOver <> inProgressEvents, gs')
  where
    gs' = Game.makeMove gs move
    gameOver = if isGameOver gs then Just [GameOver] else Nothing
    inProgressEvents = do
        let
            isFirstRound = (gs ^. #round) == 0
            players = gs ^. #players
            currentPlayer = CZ.current players
            lastPlayer = CZ.current $ CZ.left players
            turnStarting = currentPlayer ^. #tries == 0
            wasMyTurn = lastPlayer ^. #id == me
            lastPlayerCorrect = isJust $ lastPlayer ^. #lastWord
            isMyTurn = turnStarting && currentPlayer ^. #id == me
            boolToMaybe = bool Nothing . Just
            iWin = boolToMaybe [IWin] $ isGameOver gs
            myTurn = boolToMaybe [MyTurn] isMyTurn
            timeUp =
                boolToMaybe [TimeUp]
                    $ turnStarting
                    && wasMyTurn
                    && not lastPlayerCorrect
                    && not isFirstRound
        iWin <|> myTurn <|> timeUp
