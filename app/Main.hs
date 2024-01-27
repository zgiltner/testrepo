module Main (main) where

import RIO

import App (App (..))
import Game (GameState (..), initialGameState)
import Network.Wai.Handler.Warp
import qualified RIO.HashSet as HashSet
import Server (app)
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

    run 8080 $ app App{..} Nothing
