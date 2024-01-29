module Main (main) where

import RIO

import App (App (..))
import CaseInsensitive (CaseInsensitiveText (..))
import Game (GameState (..), initialSettings)
import Network.Wai.Handler.Warp
import qualified RIO.HashSet as HashSet
import qualified RIO.Text as T
import Server (app)
import System.Environment (lookupEnv)
import System.Random (newStdGen)

main :: IO ()
main = do
    wordsFile <- fromMaybe "words.txt" <$> lookupEnv "WORDS_FILE"
    wordsSet <- HashSet.fromList . fmap CaseInsensitiveText . T.lines <$> readFileUtf8 wordsFile

    givenLettersFile <- fromMaybe "histogram.csv" <$> lookupEnv "GIVEN_LETTERS_FILE"
    givenLettersSet <- take 450 . fmap (CaseInsensitiveText . T.takeWhile (/= ',')) . T.lines <$> readFileUtf8 givenLettersFile

    stdGen <- newStdGen

    wsGameState <- do
        let s =
                Left
                    $ initialSettings
                        stdGen
                        wordsSet
                        givenLettersSet
        chan <- newTChanIO
        newTVarIO (s, chan)

    wsGameStateTimer <- newTVarIO Nothing

    logOptions' <- logOptionsHandle stderr False
    let logOptions = setLogUseTime True logOptions'

    portEnv <- lookupEnv "APP_PORT"
    let port = fromMaybe 8080 $ readMaybe @Int =<< portEnv

    withLogFunc logOptions $ \logFunction -> run port $ app App{..} Nothing
