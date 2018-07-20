{-# LANGUAGE OverloadedLabels #-}

module Mintz.Service.Person where

import Data.Proxy
import Data.IORef
import System.FilePath
import System.Directory
import System.IO
import Text.Printf
import qualified Data.ByteString.Lazy as BL
import Data.Extensible
import Control.Lens hiding ((:>), firstOf)
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
                         (orderBy @Person "display_order" ASC)
                         (Just (limit, offset))
    return $ values graph

getPerson :: (With '[DB])
          => Integer
          -> IO (Maybe Person)
getPerson pid = do
    graph <- selectNodes (Proxy :: Proxy PersonList)
                         (Proxy :: Proxy Person)
                         ((==?) @Person "id" pid)
                         (../)
                         Nothing
    return $ (@< graph) <$> firstOf @Person graph

-- m [a] -> (a -> m b) -> m [b]

findIcon :: FilePath
         -> Person
         -> IO (Maybe FilePath)
findIcon dir person = do
    let path = dir </> iconFile person
    doesPathExist path >>= \b -> return $ if b then Just path else Nothing

-- TODO
-- readIORefを毎度呼ぶのが面倒。stmtとか作るのも面倒。
-- getRecordしないとlensアクセスできないのが面倒。

createPerson :: (With '[DB])
             => Person
             -> IO Person
createPerson person = do
    getDialect >>= \d -> lockTables d EXCLUSIVE ["person"]
    conn <- readIORef (contextOf @DB ?cxt) >>= return . connect

    p <- insertRecord person

    let pid = view #id (getRecord p)

    stmt <- prepare conn "UPDATE person \
                         \SET display_order = (SELECT COALESCE(MAX(display_order)+1, 1) FROM person) \
                         \WHERE id = ?"

    execute stmt [toSql pid]

    return p

type UpdatePerson = "person" :// Record (Person' ^- '["display_order"])

updatePerson :: (With '[DB])
             => UpdatePerson
             -> IO ()
updatePerson person = do
    let (graph, _) = person +< (newGraph :: Graph UpdatePerson)
    restoreGraph graph
    return ()

createIcon :: (With '[DB])
           => FilePath
           -> Integer
           -> BL.ByteString
           -> IO Bool
createIcon dir pid icon = do
    graph <- selectNodes (Proxy :: Proxy PersonList)
                         (Proxy :: Proxy Person)
                         ((==?) @Person "id" pid)
                         (../)
                         Nothing
    case (@< graph) <$> firstOf @Person graph of
        Just person -> do
            let path = dir </> iconFile person
            withBinaryFile path WriteMode $ \h -> do
                BL.hPut h icon
            return True
        Nothing -> 
            return False

iconFile :: Person
         -> FilePath
iconFile p = printf "%08d.png" (view #id $ getRecord p)