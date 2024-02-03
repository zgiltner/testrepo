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
    , freeLetters :: HashSet CaseInsensitiveChar
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
        , freeLetters = mempty
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

currentPlayerL :: Lens' GameState PlayerState
currentPlayerL = #players % currentL

makeMove :: GameState -> Move -> GameState
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

genRandom :: (Random a) => (a, a) -> (MonadState GameState m) => m a
genRandom r = #settings % #stdGen %%= randomR r

pickNewGivenLetters :: (MonadState GameState m) => m ()
pickNewGivenLetters = do
    givenLettersSet <- use $ #settings % #givenLettersSet
    i <- genRandom (0, length givenLettersSet - 1)
    #givenLetters .= givenLettersSet !! i

pickFreeLetter :: (MonadState GameState m) => CaseInsensitiveText -> m ()
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
