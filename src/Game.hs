{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Game (
    GameState (..),
    Move (..),
    PlayerState (..),
    findInZipper,
    mkMove,
    isGameOver,
    isPlayerAlive,
    isPlayerTurn,
) where

import Data.Foldable (toList)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty.Zipper (Zipper)
import qualified Data.List.NonEmpty.Zipper as Z
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import WithPlayerApi (PlayerId (..))

data GameState = GameState
    { players :: Zipper PlayerState
    , currentString :: Text
    , alreadyUsedWords :: HashSet Text
    }

data PlayerState = PlayerState
    { id :: PlayerId
    , letters :: HashSet Char
    , lives :: Int
    }
    deriving (Show)

data Move = Guess Text | TimeUp

mkMove :: GameState -> Move -> GameState
mkMove gs = \case
    Guess g
        | isValidGuess gs g ->
            gs
                { players = goToNextPlayer $ updateCurrent (validGuessForPlayer g) gs.players
                , -- TODO: Fix me
                  currentString = gs.currentString
                , alreadyUsedWords = HashSet.insert g gs.alreadyUsedWords
                }
        | otherwise -> gs
    TimeUp ->
        gs
            { players = goToNextPlayer $ updateCurrent timeUpForPlayer gs.players
            , -- TODO: Fix me
              currentString = gs.currentString
            }

validGuessForPlayer :: Text -> PlayerState -> PlayerState
validGuessForPlayer g ps =
    ps
        { lives = if hasAllLetters letters' then ps.lives + 1 else ps.lives
        , letters = if hasAllLetters letters' then mempty else letters'
        }
  where
    hasAllLetters = (== 26) . HashSet.size
    letters' = T.foldr HashSet.insert ps.letters g

timeUpForPlayer :: PlayerState -> PlayerState
timeUpForPlayer ps = ps{lives = ps.lives - 1}

goToNextPlayer :: Zipper PlayerState -> Zipper PlayerState
goToNextPlayer = findInZipper isPlayerAlive

isGameOver :: GameState -> Bool
isGameOver gs = (== 1) $ length $ filter isPlayerAlive $ toList gs.players

isPlayerAlive :: PlayerState -> Bool
isPlayerAlive ps = ps.lives > 0

isValidGuess :: GameState -> Text -> Bool
isValidGuess gs g =
    gs.currentString
        `T.isInfixOf` g
        && not (g `HashSet.member` gs.alreadyUsedWords)

isPlayerTurn :: Zipper PlayerState -> PlayerState -> Bool
isPlayerTurn z ps = (Z.current z).id == ps.id

findInZipper :: (PlayerState -> Bool) -> Zipper PlayerState -> Zipper PlayerState
findInZipper f = go
  where
    go z =
        let z' = fromMaybe (Z.start z) $ Z.right z
         in if f $ Z.current z' then z' else go z'

updateCurrent :: (a -> a) -> Zipper a -> Zipper a
updateCurrent f z = Z.replace (f $ Z.current z) z

ps1 = PlayerState (PlayerId $ fromJust $ UUID.fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c") mempty 2
ps2 = PlayerState (PlayerId $ fromJust $ UUID.fromString "550e8400-e29b-41d4-a716-446655440000") mempty 1
ps3 = PlayerState (PlayerId $ fromJust $ UUID.fromString "112640ef-5e44-4be2-8379-346ab4c66acb") mempty 2

foo = getPlayerId . (.id) . Z.current <$> iterate goToNextPlayer (Z.fromNonEmpty $ ps1 :| [ps2, ps3])

-- >>> take 5 foo
-- [c2cc10e1-57d6-4b6f-9899-38d972112d8c,550e8400-e29b-41d4-a716-446655440000,112640ef-5e44-4be2-8379-346ab4c66acb,c2cc10e1-57d6-4b6f-9899-38d972112d8c,550e8400-e29b-41d4-a716-446655440000]
