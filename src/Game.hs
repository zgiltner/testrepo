{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game (
    GameState (..),
    Settings (..),
    HasSettings (..),
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
) where

import RIO

import CaseInsensitive (CaseInsensitiveChar (..), CaseInsensitiveText)
import qualified CaseInsensitive
import CircularZipper (CircularZipper (..), findRight, updateCurrent)
import qualified CircularZipper as CZ
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import RIO.List.Partial ((!!))
import System.Random (StdGen, randomR)
import WithPlayerApi (PlayerId (..))

data Settings = Settings
    { validWords :: HashSet CaseInsensitiveText
    , givenLettersSet :: [CaseInsensitiveText]
    , stdGen :: StdGen
    , players :: HashMap PlayerId (Maybe Text)
    , secondsToGuess :: Int
    }

class HasSettings a where
    settingsL :: Lens' a Settings

instance HasSettings Settings where
    settingsL = lens id $ \_ s -> s

data StartedGameState = StartedGameState
    { players :: CircularZipper PlayerState
    , givenLetters :: CaseInsensitiveText
    , alreadyUsedWords :: HashSet CaseInsensitiveText
    , settings :: Settings
    }
instance HasSettings StartedGameState where
    settingsL = lens (.settings) $ \g settings -> g{settings}

data PlayerState = PlayerState
    { id :: PlayerId
    , name :: Maybe Text
    , letters :: HashSet CaseInsensitiveChar
    , lives :: Int
    , tries :: Int
    }
    deriving (Show)

initialPlayerState :: PlayerId -> Maybe Text -> PlayerState
initialPlayerState playerId name =
    PlayerState
        { id = playerId
        , name
        , letters = HashSet.fromList $ fmap CaseInsensitiveChar ['b' .. 'z']
        , lives = 3
        , tries = 0
        }

data GameState = GameStateUnStarted Settings | GameStateStarted StartedGameState

instance HasSettings GameState where
    settingsL =
        lens
            ( \case
                GameStateUnStarted s -> s
                GameStateStarted g -> view settingsL g
            )
            ( \gs settings -> case gs of
                GameStateUnStarted _ -> GameStateUnStarted settings
                GameStateStarted g -> GameStateStarted $ g{settings}
            )

initialGameState :: StdGen -> HashSet CaseInsensitiveText -> [CaseInsensitiveText] -> Settings
initialGameState stdGen validWords givenLettersSet = Settings{players = mempty, secondsToGuess = 4, ..}

startGame :: Settings -> GameState
startGame s = case HashMap.toList s.players of
    [] -> GameStateUnStarted s
    (p : ps) ->
        let
            (givenLetters, stdGen) = randomGivenLetters s.stdGen s.givenLettersSet
            settings = s{stdGen}
         in
            GameStateStarted
                $ StartedGameState
                    { alreadyUsedWords = mempty
                    , players = CZ.fromNonEmpty $ fmap (uncurry initialPlayerState) $ p :| ps
                    , ..
                    }

data Move = Guess CaseInsensitiveText | TimeUp

mkMove :: StartedGameState -> Move -> StartedGameState
mkMove gs = \case
    Guess g
        | isValidGuess gs g ->
            pickNewGivenLetters
                $ gs
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
pickNewGivenLetters gs =
    let (givenLetters, stdGen) = randomGivenLetters gs.settings.stdGen gs.settings.givenLettersSet
     in gs{givenLetters, settings = gs.settings{stdGen}}

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
    gs.givenLetters
        `CaseInsensitive.isInfixOf` g
        && not (g `HashSet.member` gs.alreadyUsedWords)
        && (g `HashSet.member` gs.settings.validWords)

isPlayerTurn :: CircularZipper PlayerState -> PlayerState -> Bool
isPlayerTurn z ps = (CZ.current z).id == ps.id
