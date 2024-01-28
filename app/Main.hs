module Main (main) where

import RIO

import App (App (..))
import Game (GameState (..), initialGameState)
import Network.Wai.Handler.Warp
import qualified RIO.HashSet as HashSet
import Server (app)
import System.Environment (lookupEnv)
import System.Random (mkStdGen)

main :: IO ()
main = do
    wsGameState <- do
        let s =
                GameStateUnStarted
                    $ initialGameState
                        (mkStdGen 0)
                        (HashSet.fromList ["the", "quick", "brown", "fox", "friday"])
                        ["fri", "day"]
        chan <- newTChanIO
        newTVarIO (s, chan)

    wsGameStateTimer <- newTVarIO Nothing

    logOptions' <- logOptionsHandle stderr False
    let logOptions = setLogUseTime True logOptions'
    portEnv <- lookupEnv "APP_PORT"
    let port = fromMaybe 8080 $ readMaybe @Int =<< portEnv
    withLogFunc logOptions $ \logFunction -> run port $ app App{..} Nothing
