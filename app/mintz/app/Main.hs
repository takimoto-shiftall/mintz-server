{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Network.Wai.Handler.Warp (run)
import Text.Pretty.Simple
import Mintz.HTTP.App

main :: IO ()
main = do
    app "config/develop.yml" >>= \v -> do
        case v of
            Left e -> forM_ e print
            Right (a, s) -> do
                putStrLn "mintz-server has launched with..."
                pPrint s
                run 8001 a