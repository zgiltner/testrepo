module Main (main) where

import RIO

import App (App (..))
import CaseInsensitive (CaseInsensitiveText (..))
import Game (GameState (..), initialGameState)
import Network.Wai.Handler.Warp
import qualified RIO.HashSet as HashSet
import qualified RIO.Text as T
import Server (app)
import System.Environment (lookupEnv)
import System.Random (mkStdGen)

main :: IO ()
main = do
    wordsSet <- HashSet.fromList . fmap CaseInsensitiveText . T.lines <$> readFileUtf8 "words.txt"
    givenLettersSet <- take 450 . fmap (CaseInsensitiveText . T.takeWhile (/= ',')) . T.lines <$> readFileUtf8 "histogram.csv"
    wsGameState <- do
        let s =
                GameStateUnStarted
                    $ initialGameState
                        (mkStdGen 0)
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
