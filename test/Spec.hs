{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    pure ()
