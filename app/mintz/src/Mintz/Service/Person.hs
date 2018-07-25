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

-- | Returns a list of ordered persons in specified range.
listPersons :: (With '[DB])
            => Int -- ^ Limit count of persons.
            -> Int -- ^ Offset of the order.
            -> IO [Person] -- ^ List of ordered persons.
listPersons limit offset = do
    graph <- selectNodes (Proxy :: Proxy (Graph Person))
                         (Proxy :: Proxy Person)
                         (..?)
                         (orderBy @Person "display_order" ASC)
                         (Just (limit, offset))
    return $ values graph

-- | Returns a person of specified ID if exists.
getPerson :: (With '[DB])
          => Integer -- ^ Person ID.
          -> IO (Maybe Person) -- ^ Person of the ID.
getPerson pid = do
    graph <- fetchOne @(Graph Person) pid
    return $ (@< graph) <$> firstOf @Person graph

-- | Returns a path to the icon of specified person.
findIcon :: FilePath -- ^ Path to the directory containing icons.
         -> Person -- ^ Icon owner person.
         -> IO (Maybe FilePath) -- ^ Path to the icon file if exists.
findIcon dir person = do
    let path = dir </> iconFile person
    doesPathExist path >>= \b -> return $ if b then Just path else Nothing

-- | Creates new person.
createPerson :: (With '[DB])
             => (=+)Person -- ^ Person to create having its properties.
             -> IO ((=+)Person) -- ^ Created person having newly assigned ID.
createPerson person = do
    getDialect >>= \d -> lockTables d EXCLUSIVE ["person"]

    p <- insertOne person

    executeSQL_ "UPDATE person \
                \SET display_order = (SELECT COALESCE(MAX(display_order)+1, 1) FROM person) \
                \WHERE id = ?"
                $ view #id p

    return p

type UpdatePerson = (=/)Person :^- '["display_order"]

-- | Updates an existing person.
updatePerson :: (With '[DB])
             => UpdatePerson -- ^ Person to update.
             -> IO () -- ^ Returns nothing.
updatePerson person = do
    updateOne person

-- | Create icon file of specified person.
--
-- Although this system can use icon in PNG format, this function does not check data format.
-- Therefore, this function returns @True@ even if the data format is not PNG.
createIcon :: (With '[DB])
           => FilePath -- ^ Path of directory containing icons.
           -> Integer -- ^ Person ID.
           -> BL.ByteString -- ^ Icon data in PNG format.
           -> IO Bool -- ^ True when icon is created successfully.
createIcon dir pid icon = do
    graph <- fetchOne @(Graph Person) pid
    case (@< graph) <$> firstOf @Person graph of
        Just person -> do
            let path = dir </> iconFile person
            withBinaryFile path WriteMode $ \h -> do
                BL.hPut h icon
            return True
        Nothing -> 
            return False

-- ------------------------------------------------------------
-- Private 
-- ------------------------------------------------------------

-- | Generate path to icon file of the person.
iconFile :: Person -- ^ Person.
         -> FilePath -- ^ Path to icon file.
iconFile p = printf "%08d.png" (view #id p)