{-# LANGUAGE OverloadedLabels #-}

module Mintz.Service.Log where

import Data.Proxy
import Data.IORef
import Data.Extensible
import Control.Monad.State
import Control.Lens hiding ((:>))
import Data.Resource
import Data.Model.Graph
import Database.HDBC
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Mintz.Model.Models
import Mintz.Settings (DB)

type LogInsertGraph = Graph ((=+)PublishLog)
                       :><: (=+)CalledPerson
                       :><: Person
                       :><: ((=+)CalledPerson :- (=+)PublishLog)
                       :><: ((=+)CalledPerson :- Person)

createLog :: (With '[DB])
          => (=+)PublishLog
          -> [Person]
          -> IO LogInsertGraph
createLog log persons = do
    (_, graph) <- (`runStateT` (newGraph :: LogInsertGraph)) $ do
        lc <- (+<<) log
        forM_ persons $ \p -> do
            pc <- (+<<) p
            cpc <- (+<<) (Model emptyRecord :: (=+)CalledPerson)
            cpc -*< lc
            cpc -*< pc
    restoreGraph graph

type AudioHash = ExtraModel '["audio_hash" :> String] '[]

fetchOldHashes :: (With '[DB])
               => Integer
               -> IO [String]
fetchOldHashes c
    | c <= 0    = return []
    | otherwise = do
        g <- selectQuery (Proxy :: Proxy (Graph AudioHash))
                         (Proxy :: Proxy '[AudioHash])
                         "SELECT audio_hash \
                         \FROM publish_log \
                         \GROUP BY audio_hash \
                         \ORDER BY MAX(published_at) DESC \
                         \LIMIT ? OFFSET 0"
                         (holderValues c)
        return $ map (view #audio_hash) (valuesOf g :: [AudioHash])
