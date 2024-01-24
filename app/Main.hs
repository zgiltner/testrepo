module Main (main) where

import Lib (app)
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8080 $ app Nothing
