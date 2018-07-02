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

type LogInsertGraph = Graph PublishLog
                       :><: CalledPerson
                       :><: Person'S
                       :><: (CalledPerson :- PublishLog)
                       :><: (CalledPerson :- Person'S)

createLog :: (With '[DB])
          => PublishLog
          -> [Person'S]
          -> IO LogInsertGraph
createLog log persons = do
    (_, graph) <- (`runStateT` (newGraph :: LogInsertGraph)) $ do
        lc <- (+<<) log
        forM_ persons $ \p -> do
            pc <- (+<<) p
            cpc <- (+<<) (Model emptyRecord :: CalledPerson)
            cpc -*< lc
            cpc -*< pc
    restoreGraph graph
