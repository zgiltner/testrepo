{-# LANGUAGE DerivingStrategies #-}

module DerivingStrategies (
    Settings (..),
    GameState (..),
    PlayerState (..)
) where

import CustomPrelude
import CaseInsensitive (CaseInsensitiveChar, CaseInsensitiveText)
import RIO.HashMap ()
import RIO.HashSet ()
import WithPlayerApi (PlayerId)
import System.Random (StdGen)
import CircularZipper (CircularZipper)

data Settings = Settings
    { validWords :: HashSet CaseInsensitiveText
    , givenLettersSet :: [CaseInsensitiveText]
    , stdGen :: StdGen
    , players :: HashMap PlayerId (Maybe Text)
    , secondsToGuess :: Int
    }
    deriving stock (Show, Generic)

data GameState = GameState
    { gamePlayers :: CircularZipper PlayerState
    , givenLetters :: CaseInsensitiveText
    , alreadyUsedWords :: HashSet CaseInsensitiveText
    , settings :: Settings
    , round :: Natural
    }
    deriving stock (Show, Generic)

data PlayerState = PlayerState
    { id :: PlayerId
    , name :: Maybe Text
    , letters :: HashSet CaseInsensitiveChar
    , lives :: Int
    , tries :: Int
    , lastWord :: Maybe CaseInsensitiveText
    , freeLetters :: HashSet CaseInsensitiveChar
    }
    deriving stock (Show, Generic)
