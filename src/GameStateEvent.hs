{-# LANGUAGE OverloadedLabels #-}

module GameStateEvent (GameStateEvent (..), getGameStateEvents) where

import App (Game (..))
import qualified CircularZipper as CZ
import Control.Applicative (empty)
import CustomPrelude
import Data.Aeson (ToJSON (..))
import Game (GameState (..), PlayerState (..), isGameOver)
import Servant (ToHttpApiData (..))
import WithPlayerApi (PlayerId)

data GameStateEvent
    = IWin
    | MyTurn
    | TimeUp
    | GameOver
    | WrongGuess
    | CorrectGuess
    | SettingsUpdate
    | ILose
    deriving stock (Show)

instance ToHttpApiData GameStateEvent where
    toUrlPiece = tshow

instance ToJSON GameStateEvent where
    toJSON = toJSON . toUrlPiece
    toEncoding = toEncoding . toUrlPiece

pureIf :: (Alternative m) => Bool -> a -> m a
pureIf a b = if a then pure b else empty

getGameStateEvents :: PlayerId -> Game -> Maybe [GameStateEvent]
getGameStateEvents me = \case
    InLobby _ -> Just [SettingsUpdate]
    InGame gs -> do
        let
            isFirstRound = 0 == gs ^. #round
            currentPlayer = gs ^. #players % to CZ.current
            lastPlayer = gs ^. #players % to (CZ.current . CZ.left)
            turnStarting = currentPlayer ^. #tries == 0
            wasMyTurn = lastPlayer ^. #id == me
            lastPlayerCorrect = isJust $ lastPlayer ^. #lastWord
            isMyTurn = turnStarting && currentPlayer ^. #id == me

            gameOver = pureIf (isGameOver gs) [GameOver]
            iWin = pureIf (isGameOver gs && isMyTurn) [IWin]
            myTurn = pureIf isMyTurn [MyTurn]
            timeUp = pureIf (turnStarting && wasMyTurn && not lastPlayerCorrect && not isFirstRound) [TimeUp]
            iLose = pureIf (isGameOver gs && not isMyTurn) [ILose]

        gameOver <> (iWin <|> iLose <|> myTurn <|> timeUp)
