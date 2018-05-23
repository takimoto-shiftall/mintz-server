{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Mintz.Model.Types where

import GHC.Generics
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Data.Aeson
import Data.Convertible
import Text.Parsec
import Database.HDBC
import Database.ORM
import Mintz.Settings (Database(..))

data LangType = EN | MB deriving (Show, Eq)

data Label = Label { en_label :: String
                   , mb_label :: String
                   , en_reading :: String
                   , mb_reading :: String
                   } deriving (Show, Read, Eq, Generic)

instance Convertible Label SqlValue where
    safeConvert (Label {..}) = safeConvert $ "('" ++ L.intercalate "','" (map esc [en_label, mb_label, en_reading, mb_reading]) ++ "')"
        where
            esc "" = ""
            esc ('\"':cs) = "\\\"" ++ (esc cs)
            esc ('\\':cs) = "\\\\" ++ (esc cs)
            esc (c:cs) = c:(esc cs)

instance Convertible SqlValue Label where
    safeConvert (SqlByteString x) = do
        let Right (en:mb:enr:mbr:[]) = parse parseTuple "" (U.toString x)
        return $ Label en mb enr mbr

data Lang = Lang { en :: String
                 , mb :: String
                 } deriving (Show, Read, Eq, Generic)

instance Convertible Lang SqlValue where
    safeConvert (Lang {..}) = safeConvert $ "('" ++ L.intercalate "','" (map esc [en, mb]) ++ "')"
        where
            esc "" = ""
            esc ('\"':cs) = "\\\"" ++ (esc cs)
            esc ('\\':cs) = "\\\\" ++ (esc cs)
            esc (c:cs) = c:(esc cs)

parseTuple :: Parsec String u [String]
parseTuple = do
    char '('
    manyTill (parseOne <* spaces <* optional (char ',') <* spaces) (char ')')
    where
        parseOne :: Parsec String u String
        parseOne = do
            char '\''
            manyTill anyChar (char '\'' <* notFollowedBy (char '\''))

instance Convertible SqlValue Lang where
    safeConvert (SqlByteString x) = do
        let Right (en:mb:[]) = parse parseTuple "" (U.toString x)
        return $ Lang en mb

instance TypeMappable Database where
    mapColumnType _ "jsonb" _ = [t| String |]
    mapColumnType _ "USER-DEFINED" "label" = [t| Label |]
    mapColumnType _ "USER-DEFINED" "lang" = [t| Lang |]
    mapColumnType (Database pg) typ udt = mapColumnType pg typ udt

--SELECT
--  c.column_name, c.data_type, c.is_nullable, c.column_default, c.udt_name,
--  e.data_type AS element_type
--FROM
--  information_schema.columns AS c
--  LEFT JOIN information_schema.element_types AS e
--    ON ((c.table_catalog, c.table_schema, c.table_name, 'TABLE', c.dtd_identifier)
--      = (e.object_catalog, e.object_schema, e.object_name, e.object_type, e.collection_type_identifier))
--WHERE c.table_name = 'person' ORDER BY c.ordinal_position

data TypeTalk = TypeTalk { name :: String
                         } deriving (Show, Eq, Generic)

instance FromJSON TypeTalk where
instance ToJSON TypeTalk where

data Notifications = Notifications { type_talk :: Maybe TypeTalk
                                   } deriving (Show, Eq, Generic)

instance FromJSON Notifications where
instance ToJSON Notifications where
