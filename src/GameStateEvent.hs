{-# LANGUAGE OverloadedLabels #-}

module GameStateEvent (GameStateEvent (..), getGameStateEvents) where

import App (Game (..))
import qualified CircularZipper as CZ
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
    deriving stock (Show)

instance ToHttpApiData GameStateEvent where
    toUrlPiece = tshow

instance ToJSON GameStateEvent where
    toJSON = toJSON . toUrlPiece
    toEncoding = toEncoding . toUrlPiece

boolToMaybe :: a -> Bool -> Maybe a
boolToMaybe = bool Nothing . Just

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

            gameOver = boolToMaybe [GameOver] $ isGameOver gs
            iWin = boolToMaybe [IWin] $ isGameOver gs && isMyTurn
            myTurn = boolToMaybe [MyTurn] isMyTurn
            timeUp = boolToMaybe [TimeUp] $ turnStarting && wasMyTurn && not lastPlayerCorrect && not isFirstRound

        gameOver <> (iWin <|> myTurn <|> timeUp)
