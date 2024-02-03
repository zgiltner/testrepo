{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game (
    GameState (..),
    Settings (..),
    Move (..),
    PlayerState (..),
    initialPlayerState,
    initialSettings,
    startGame,
    makeMove,
    isGameOver,
    isPlayerAlive,
    isPlayerTurn,
) where

import CustomPrelude

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
    deriving (Show, Generic)

data GameState = GameState
    { players :: CircularZipper PlayerState
    , givenLetters :: CaseInsensitiveText
    , alreadyUsedWords :: HashSet CaseInsensitiveText
    , settings :: Settings
    , round :: Natural
    }
    deriving (Show, Generic)

data PlayerState = PlayerState
    { id :: PlayerId
    , name :: Maybe Text
    , letters :: HashSet CaseInsensitiveChar
    , lives :: Int
    , tries :: Int
    , lastWord :: Maybe CaseInsensitiveText
    }
    deriving (Show, Generic)

initialPlayerState :: PlayerId -> Maybe Text -> PlayerState
initialPlayerState playerId name =
    PlayerState
        { id = playerId
        , name
        , letters = mempty
        , lives = 3
        , tries = 0
        , lastWord = Nothing
        }

initialSettings :: StdGen -> HashSet CaseInsensitiveText -> [CaseInsensitiveText] -> Settings
initialSettings stdGen validWords givenLettersSet = Settings{players = mempty, secondsToGuess = 7, ..}

startGame :: Settings -> Maybe GameState
startGame s = case HashMap.toList (s ^. #players) of
    [] -> Nothing
    (p : ps) ->
        let
            (givenLetters, stdGen) = randomGivenLetters (s ^. #stdGen) (s ^. #givenLettersSet)
            settings = s{stdGen}
         in
            Just
                $ GameState
                    { alreadyUsedWords = mempty
                    , players = CZ.fromNonEmpty $ fmap (uncurry initialPlayerState) $ p :| ps
                    , round = 0
                    , ..
                    }

data Move = Guess CaseInsensitiveText | TimeUp

makeMove :: GameState -> Move -> GameState
makeMove gs = \case
    Guess g
        | isValidGuess gs g ->
            pickNewGivenLetters
                $ gs
                & (#players %~ goToNextPlayer . updateCurrent (validGuessForPlayer g))
                & (#alreadyUsedWords %~ HashSet.insert g)
                & (#round %~ (+ 1))
        | otherwise -> gs & #players %~ updateCurrent (#tries %~ (+ 1))
    TimeUp ->
        gs
            & (#players %~ goToNextPlayer . updateCurrent timeUpForPlayer)
            & (#round %~ (+ 1))

pickNewGivenLetters :: GameState -> GameState
pickNewGivenLetters gs =
    let (givenLetters, stdGen) = randomGivenLetters (gs ^. #settings % #stdGen) (gs ^. #settings % #givenLettersSet)
     in gs
            & (#givenLetters .~ givenLetters)
            & (#settings % #stdGen .~ stdGen)

randomGivenLetters :: StdGen -> [CaseInsensitiveText] -> (CaseInsensitiveText, StdGen)
randomGivenLetters stdGen givenLettersSet = let (i, stdGen') = randomR (0, length givenLettersSet - 1) stdGen in (givenLettersSet !! i, stdGen')

validGuessForPlayer :: CaseInsensitiveText -> PlayerState -> PlayerState
validGuessForPlayer g ps =
    ps
        { lives = if hasAllLetters then ps ^. #lives + 1 else ps ^. #lives
        , letters = if hasAllLetters then mempty else letters
        , tries = 0
        , lastWord = Just g
        }
  where
    hasAllLetters = (== 26) $ HashSet.size letters
    letters = HashSet.union (ps ^. #letters) $ CaseInsensitive.caseInsensitiveLetters g

timeUpForPlayer :: PlayerState -> PlayerState
timeUpForPlayer ps = ps{lives = ps ^. #lives - 1, tries = 0}

goToNextPlayer :: CircularZipper PlayerState -> CircularZipper PlayerState
goToNextPlayer z = updateCurrent (set #lastWord Nothing) $ fromMaybe z $ findRight isPlayerAlive z

isGameOver :: GameState -> Bool
isGameOver gs = (== 1) $ length $ filter isPlayerAlive $ toList $ gs ^. #players

isPlayerAlive :: PlayerState -> Bool
isPlayerAlive ps = ps ^. #lives > 0

isValidGuess :: GameState -> CaseInsensitiveText -> Bool
isValidGuess gs g =
    ((gs ^. #givenLetters) `CaseInsensitive.isInfixOf` g)
        && not (g `HashSet.member` (gs ^. #alreadyUsedWords))
        && (g `HashSet.member` (gs ^. #settings % #validWords))

isPlayerTurn :: CircularZipper PlayerState -> PlayerState -> Bool
isPlayerTurn z ps = CZ.current z ^. #id == ps ^. #id
