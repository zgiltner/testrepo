{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game (
    GameState (..),
    UnStartedGameState (..),
    StartedGameState (..),
    Move (..),
    PlayerState (..),
    initialPlayerState,
    initialGameState,
    startGame,
    mkMove,
    isGameOver,
    isPlayerAlive,
    isPlayerTurn,
    gameStateMicrosecondsToGuess,
) where

import CaseInsensitive (CaseInsensitiveChar (..), CaseInsensitiveText)
import qualified CaseInsensitive
import CircularZipper (CircularZipper (..), findRight, updateCurrent)
import qualified CircularZipper as CZ
import Data.Foldable (toList)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import System.Random (StdGen, randomR)
import WithPlayerApi (PlayerId (..))

data UnStartedGameState = UnStartedGameState
    { validWords :: HashSet CaseInsensitiveText
    , givenLettersSet :: [CaseInsensitiveText]
    , stdGen :: StdGen
    , players :: HashSet PlayerId
    , microsecondsToGuess :: Int
    }

data StartedGameState = StartedGameState
    { players :: CircularZipper PlayerState
    , givenLetters :: CaseInsensitiveText
    , alreadyUsedWords :: HashSet CaseInsensitiveText
    , validWords :: HashSet CaseInsensitiveText
    , givenLettersSet :: [CaseInsensitiveText]
    , stdGen :: StdGen
    , microsecondsToGuess :: Int
    }

data PlayerState = PlayerState
    { id :: PlayerId
    , letters :: HashSet CaseInsensitiveChar
    , lives :: Int
    , tries :: Int
    }
    deriving (Show)
initialPlayerState :: PlayerId -> PlayerState
initialPlayerState playerId =
    PlayerState
        { id = playerId
        , letters = HashSet.fromList $ fmap CaseInsensitiveChar ['b' .. 'z']
        , lives = 3
        , tries = 0
        }

data GameState = GameStateUnStarted UnStartedGameState | GameStateStarted StartedGameState

gameStateMicrosecondsToGuess :: GameState -> Int
gameStateMicrosecondsToGuess = \case
    GameStateUnStarted g -> g.microsecondsToGuess
    GameStateStarted g -> g.microsecondsToGuess

initialGameState :: StdGen -> HashSet CaseInsensitiveText -> [CaseInsensitiveText] -> UnStartedGameState
initialGameState stdGen validWords givenLettersSet = UnStartedGameState{players = mempty, microsecondsToGuess = 4000000, ..}

startGame :: UnStartedGameState -> GameState
startGame uGs@UnStartedGameState{..} = case toList players of
    [] -> GameStateUnStarted uGs
    (p : ps) ->
        let
            (givenLetters, stdGen') = randomGivenLetters stdGen givenLettersSet
         in
            GameStateStarted $
                StartedGameState
                    { alreadyUsedWords = mempty
                    , stdGen = stdGen'
                    , players = CZ.fromNonEmpty $ fmap initialPlayerState $ p :| ps
                    , ..
                    }

data Move = Guess CaseInsensitiveText | TimeUp

mkMove :: StartedGameState -> Move -> StartedGameState
mkMove gs = \case
    Guess g
        | isValidGuess gs g ->
            pickNewGivenLetters $
                gs
                    { players = goToNextPlayer $ updateCurrent (validGuessForPlayer g) gs.players
                    , alreadyUsedWords = HashSet.insert g gs.alreadyUsedWords
                    }
        | otherwise ->
            gs
                { players = updateCurrent (\ps -> ps{tries = ps.tries + 1}) gs.players
                }
    TimeUp ->
        gs
            { players = goToNextPlayer $ updateCurrent timeUpForPlayer gs.players
            }

pickNewGivenLetters :: StartedGameState -> StartedGameState
pickNewGivenLetters gs = let (givenLetters, stdGen) = randomGivenLetters gs.stdGen gs.givenLettersSet in gs{givenLetters, stdGen}

randomGivenLetters :: StdGen -> [CaseInsensitiveText] -> (CaseInsensitiveText, StdGen)
randomGivenLetters stdGen givenLettersSet = let (i, stdGen') = randomR (0, length givenLettersSet - 1) stdGen in (givenLettersSet !! i, stdGen')

validGuessForPlayer :: CaseInsensitiveText -> PlayerState -> PlayerState
validGuessForPlayer g ps =
    ps
        { lives = if hasAllLetters then ps.lives + 1 else ps.lives
        , letters = if hasAllLetters then mempty else letters
        , tries = 0
        }
  where
    hasAllLetters = (== 26) $ HashSet.size letters
    letters = HashSet.union ps.letters $ CaseInsensitive.caseInsensitiveLetters g

timeUpForPlayer :: PlayerState -> PlayerState
timeUpForPlayer ps = ps{lives = ps.lives - 1, tries = 0}

goToNextPlayer :: CircularZipper PlayerState -> CircularZipper PlayerState
goToNextPlayer z = fromMaybe z $ findRight isPlayerAlive z

isGameOver :: StartedGameState -> Bool
isGameOver gs = (== 1) $ length $ filter isPlayerAlive $ toList gs.players

isPlayerAlive :: PlayerState -> Bool
isPlayerAlive ps = ps.lives > 0

isValidGuess :: StartedGameState -> CaseInsensitiveText -> Bool
isValidGuess gs g =
    gs.givenLetters `CaseInsensitive.isInfixOf` g
        && not (g `HashSet.member` gs.alreadyUsedWords)
        && (g `HashSet.member` gs.validWords)

isPlayerTurn :: CircularZipper PlayerState -> PlayerState -> Bool
isPlayerTurn z ps = (CZ.current z).id == ps.id
