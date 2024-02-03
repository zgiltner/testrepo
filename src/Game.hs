{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game (
    Game.GameState (..),
    Game.Settings (..),
    Move (..),
    Game.PlayerState (..),
    initialPlayerState,
    initialSettings,
    startGame,
    makeMove,
    isGameOver,
    isPlayerAlive,
    isPlayerTurn,
) where

import CustomPrelude

import CaseInsensitive (CaseInsensitiveChar (..), CaseInsensitiveText, caseInsensitiveLetters)
import qualified CaseInsensitive
import CircularZipper (CircularZipper (..), currentL, findRight, updateCurrent)
import qualified CircularZipper as CZ
import Control.Monad.State (execState)
import Control.Monad.State.Strict (MonadState)
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import RIO.List.Partial ((!!))
import System.Random (Random, StdGen, randomR)
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
    { players :: CircularZipper Game.PlayerState
    , givenLetters :: CaseInsensitiveText
    , alreadyUsedWords :: HashSet CaseInsensitiveText
    , settings :: Game.Settings
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
    , freeLetters :: HashSet CaseInsensitiveChar
    }
    deriving (Show, Generic)

initialPlayerState :: PlayerId -> Maybe Text -> Game.PlayerState
initialPlayerState playerId name =
    Game.PlayerState
        { id = playerId
        , name
        , letters = mempty
        , lives = 3
        , tries = 0
        , lastWord = Nothing
        , freeLetters = mempty
        }

initialSettings :: StdGen -> HashSet CaseInsensitiveText -> [CaseInsensitiveText] -> Game.Settings
initialSettings stdGen validWords givenLettersSet = Game.Settings{players = mempty, secondsToGuess = 7, ..}

startGame :: Game.Settings -> Maybe Game.GameState
startGame s = case HashMap.toList (s ^. #players) of
    [] -> Nothing
    (p : ps) ->
        let
            (givenLetters, stdGen) = randomGivenLetters (s ^. #stdGen) (s ^. #givenLettersSet)
            settings :: Game.Settings
            settings = s { stdGen = stdGen }
         in
            Just
                $ Game.GameState
                    { alreadyUsedWords = mempty
                    , Game.players = CZ.fromNonEmpty $ fmap (uncurry initialPlayerState) $ p :| ps
                    , round = 0
                    , ..
                    }

data Move = Guess CaseInsensitiveText | TimeUp

currentPlayerL :: Lens' Game.GameState Game.PlayerState
currentPlayerL = #players % currentL

makeMove :: Game.GameState -> Move -> Game.GameState
makeMove gs =
    flip execState gs . \case
        Guess guess
            | isValidGuess gs guess -> do
                when (CaseInsensitive.length guess >= 11) $ pickFreeLetter guess
                zoom currentPlayerL $ do
                    #tries .= 0
                    #lastWord ?= guess
                    letters <- #letters <%= HashSet.union (caseInsensitiveLetters guess)
                    let hasAllLetters = 26 == HashSet.size letters
                    when hasAllLetters $ do
                        #lives += 1
                        #letters .= mempty
                        #freeLetters .= mempty
                #players %= goToNextPlayer
                #alreadyUsedWords %= HashSet.insert guess
                #round += 1
                pickNewGivenLetters
            | otherwise -> currentPlayerL % #tries += 1
        TimeUp -> do
            currentPlayerL % #lives -= 1
            currentPlayerL % #tries .= 0
            #players %= goToNextPlayer
            #round += 1

genRandom :: (Random a) => (a, a) -> (MonadState Game.GameState m) => m a
genRandom r = #settings % #stdGen %%= randomR r

pickNewGivenLetters :: (MonadState Game.GameState m) => m ()
pickNewGivenLetters = do
    givenLettersSet <- use $ #settings % #givenLettersSet
    i <- genRandom (0, length givenLettersSet - 1)
    #givenLetters .= givenLettersSet !! i

pickFreeLetter :: (MonadState Game.GameState m) => CaseInsensitiveText -> m ()
pickFreeLetter guess = do
    freeLetters <- use $ currentPlayerL % #freeLetters
    letters <- use $ currentPlayerL % #letters
    let
        allLetters = HashSet.fromList $ CaseInsensitiveChar <$> ['A' .. 'Z']
        openLetters = foldr (flip HashSet.difference) allLetters [freeLetters, letters, caseInsensitiveLetters guess]
    i <- genRandom (0, length openLetters - 1)
    let letter = toList openLetters !! i
    currentPlayerL % #freeLetters %= HashSet.insert letter
    currentPlayerL % #letters %= HashSet.insert letter

randomGivenLetters :: StdGen -> [CaseInsensitiveText] -> (CaseInsensitiveText, StdGen)
randomGivenLetters stdGen givenLettersSet = let (i, stdGen') = randomR (0, length givenLettersSet - 1) stdGen in (givenLettersSet !! i, stdGen')

goToNextPlayer :: CircularZipper Game.PlayerState -> CircularZipper Game.PlayerState
goToNextPlayer z = updateCurrent (set #lastWord Nothing) $ fromMaybe z $ findRight isPlayerAlive z

isGameOver :: Game.GameState -> Bool
isGameOver gs = (== 1) $ length $ filter isPlayerAlive $ toList $ gs ^. #players

isPlayerAlive :: Game.PlayerState -> Bool
isPlayerAlive ps = ps ^. #lives > 0

isValidGuess :: Game.GameState -> CaseInsensitiveText -> Bool
isValidGuess gs g =
    ((gs ^. #givenLetters) `CaseInsensitive.isInfixOf` g)
        && not (g `HashSet.member` (gs ^. #alreadyUsedWords))
        && (g `HashSet.member` (gs ^. #settings % #validWords))

isPlayerTurn :: CircularZipper Game.PlayerState -> Game.PlayerState -> Bool
isPlayerTurn z ps = CZ.current z ^. #id == ps ^. #id
