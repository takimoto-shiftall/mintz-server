{-# LANGUAGE OverloadedLabels #-}

module Mintz.Service.Person where

import Data.Proxy
import Data.IORef
import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Resource
import Data.Model.Graph
import Database.HDBC
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Mintz.Model.Models
import Mintz.Settings (DB)

type PersonList = Graph Person

listPersons :: (With '[DB])
            => Int
            -> Int
            -> IO [Person]
listPersons limit offset = do
    graph <- selectNodes (Proxy :: Proxy PersonList)
                         (Proxy :: Proxy Person)
                         (..?)
                         (orderBy @Person "id" ASC)
                         (Just (limit, offset))
    return $ values graph

-- TODO
-- readIORefを毎度呼ぶのが面倒。stmtとか作るのも面倒。
-- getRecordしないとlensアクセスできないのが面倒。

createPerson :: (With '[DB])
             => Person
             -> IO Integer
createPerson person = do
    getDialect >>= \d -> lockTables d EXCLUSIVE ["person"]
    conn <- readIORef (contextOf @DB ?cxt) >>= return . connect

    p <- insertRecord person

    let pid = view #id (getRecord p)

    stmt <- prepare conn "UPDATE person \
                         \SET display_order = (SELECT COALESCE(MAX(display_order)+1, 1) FROM person) \
                         \WHERE id = ?"

    execute stmt [toSql pid]

    return pid