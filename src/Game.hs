{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game (
    GameState (..),
    Move (..),
    PlayerState (..),
    mkMove,
    isGameOver,
    isPlayerAlive,
    isPlayerTurn,
) where

import CircularZipper (CircularZipper (..), findRight, updateCurrent)
import qualified CircularZipper as CZ
import Data.Foldable (toList)
import Data.Function (on)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import UpperCase (UpperCase (..))
import WithPlayerApi (PlayerId (..))

data GameState = GameState
    { players :: CircularZipper PlayerState
    , currentString :: UpperCase
    , alreadyUsedWords :: HashSet UpperCase
    }

data PlayerState = PlayerState
    { id :: PlayerId
    , letters :: HashSet Char
    , lives :: Int
    }
    deriving (Show)

data Move = Guess UpperCase | TimeUp

mkMove :: GameState -> Move -> GameState
mkMove gs = \case
    Guess g
        | isValidGuess gs g ->
            gs
                { players = goToNextPlayer $ updateCurrent (validGuessForPlayer g) gs.players
                , alreadyUsedWords = HashSet.insert g gs.alreadyUsedWords
                }
        | otherwise -> gs
    TimeUp ->
        gs
            { players = goToNextPlayer $ updateCurrent timeUpForPlayer gs.players
            }

validGuessForPlayer :: UpperCase -> PlayerState -> PlayerState
validGuessForPlayer g ps =
    ps
        { lives = if hasAllLetters then ps.lives + 1 else ps.lives
        , letters = if hasAllLetters then mempty else letters
        }
  where
    hasAllLetters = (== 26) $ HashSet.size letters
    letters = T.foldr HashSet.insert ps.letters $ getUpperCase g

timeUpForPlayer :: PlayerState -> PlayerState
timeUpForPlayer ps = ps{lives = ps.lives - 1}

goToNextPlayer :: CircularZipper PlayerState -> CircularZipper PlayerState
goToNextPlayer z = fromMaybe z $ findRight isPlayerAlive z

isGameOver :: GameState -> Bool
isGameOver gs = (== 1) $ length $ filter isPlayerAlive $ toList gs.players

isPlayerAlive :: PlayerState -> Bool
isPlayerAlive ps = ps.lives > 0

isValidGuess :: GameState -> UpperCase -> Bool
isValidGuess gs g =
    (T.isInfixOf `on` getUpperCase) gs.currentString g
        && not (g `HashSet.member` gs.alreadyUsedWords)

isPlayerTurn :: CircularZipper PlayerState -> PlayerState -> Bool
isPlayerTurn z ps = (CZ.current z).id == ps.id
