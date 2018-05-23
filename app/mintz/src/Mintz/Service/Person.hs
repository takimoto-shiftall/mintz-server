module Mintz.Service.Person where

import Data.Proxy
import Data.Extensible
import Control.Lens hiding ((:>))
import Data.Resource
import Data.Model.Graph
import Database.ORM
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

createPerson :: (With '[DB])
             => Person
             -> IO Person
createPerson person = insertRecord person