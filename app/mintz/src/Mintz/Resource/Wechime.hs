{-# LANGUAGE RecordWildCards #-}

module Mintz.Resource.Wechime where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.List as L
import Data.IORef
import System.Directory
import System.FilePath
import System.IO
import Data.Resource
import Debug.Trace

data Chime = Chime1
           | Chime2
           deriving (Eq, Show)

data Wechime = Wechime { gpioDirectory :: String
                       , gpioChime1 :: Int
                       , gpioChime2 :: Int
                       , gpioChime12 :: Int
                       , gpioLock :: MVar Bool
                       }
             | WechimeUnavailable

gpioForChimes :: Wechime
              -> [Chime]
              -> Maybe Int
gpioForChimes WechimeUnavailable _ = Nothing
gpioForChimes (Wechime {..}) cs = 
    case map (`elem` cs) [Chime1, Chime2] of
        [True, True] -> Just gpioChime12
        [True, False] -> Just gpioChime1
        [False, True] -> Just gpioChime2
        _ -> Nothing

initWechime :: String -> Int -> Int -> Int -> IO (IORef Wechime)
initWechime dir c1 c2 c12 = do
    available <- doesPathExist dir
    if available
        then do
            -- forM_ [c1, c2, c12] $ exportOutGpio dir
            lock <- newEmptyMVar :: IO (MVar Bool)
            newIORef $ Wechime dir c1 c2 c12 lock
        else
            newIORef WechimeUnavailable

exportOutGpio :: String
              -> Int
              -> IO ()
exportOutGpio dir c = do
    withFile (dir </> "export") WriteMode $ \h -> do
        hPutStr h $ show c
        threadDelay 100000
    withFile (dir </> "gpio" ++ show c </> "direction") WriteMode $ \h -> do
        hPutStr h "out"

unexportOutGpio :: String
                -> Int
                -> IO ()
unexportOutGpio dir c = do
    withFile (dir </> "unexport") WriteMode $ \h -> do
        hPutStr h $ show c

data WechimeContext = WechimeContext Wechime

instance Resource Wechime where
    type ContextType Wechime = WechimeContext

    newContext ref = liftIO $ do
        wc <- readIORef ref
        newIORef $ WechimeContext wc

instance ResourceContext WechimeContext where
    type ResourceType WechimeContext = Wechime

    closeContext cxt status = return cxt
    execContext ref action = action

outputChime :: FilePath
            -> Maybe Int
            -> IO ()
outputChime _ Nothing = return ()
outputChime dir (Just gpio) = do
    let path = dir </> "gpio" ++ show gpio </> "value"
    withFile path WriteMode $ \h -> do
        hPutChar h '0'
        hFlush h
        threadDelay 1000000
        hPutChar h '1'
        hFlush h
        threadDelay 1000000

execChime :: (With '[WechimeContext])
          => [Chime]
          -> IO ()
execChime [] = return ()
execChime cs = do
    cxt <- readIORef $ contextOf @WechimeContext ?cxt
    case cxt of
        WechimeContext WechimeUnavailable -> do
            logCD ?cxt $ "Wechime is not available"
            return ()
        WechimeContext wc@(Wechime {..}) -> do
            logCD ?cxt $ "Wechime is available, try to run it via virtual file"
            forkIO $ bracket
                        (do
                            tryPutMVar gpioLock True
                        )
                        (\idle -> do
                            tryTakeMVar gpioLock
                        )
                        (\idle -> do
                            if idle
                                then do
                                    outputChime gpioDirectory $ gpioForChimes wc cs
                                    return ()
                                else
                                    logCD ?cxt $ "Wechime is busy now, the request is skipped"
                        )
            return ()