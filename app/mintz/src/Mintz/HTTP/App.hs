{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mintz.HTTP.App where

import Control.Exception.Safe
import Data.IORef
import qualified Data.Map as M
import Control.Monad.Logger
import System.Log.FastLogger
import Data.Proxy
import Data.Yaml
import qualified Data.Vault.Lazy as V
import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles
import Network.Wai (Application)
import Database.Redis
import Data.Resource
import Database.HDBC
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Ext.Servant.Action
import Ext.Servant.Context
import Ext.Servant.Combinators
import Mintz.Settings
import Mintz.HTTP.Site.Person
import Mintz.HTTP.API.Publish
import Mintz.HTTP.API.Person
import Mintz.HTTP.API.Voice
import Mintz.Resource.Redis
import Mintz.Resource.OpenJTalk
import Mintz.Resource.TypeTalk
import Mintz.Resource.Wechime

type ResourceTypes = '[DBResource Database, RedisPubSub, OpenJTalk, TypeTalkBot, Wechime, LoggingResource]

type ResourceAPI = (@>) ResourceTypes SiteKeys
                    :> ( "site"
                        :> ( PersonSite
                           )
                    :<|> "api"
                        :> CrossDomain '[ 'GET, 'POST, 'PUT, 'DELETE, 'OPTIONS ]
                        :> ( PublishAPI
                        :<|> PersonAPI
                        :<|> VoiceAPI
                           )
                       )

type AllAPI = ResourceAPI
         :<|> "public" :> Raw

resourceServer sc = personSite sc
               :<|> ( publishAPI sc
                 :<|> personAPI sc
                 :<|> voiceAPI sc
                    )

makeResources :: AppSettings
              -> IO (Resources (Refs ResourceTypes))
makeResources settings = do
    lr <- newLoggingResource [(anyTag, LevelDebug, LogStdout defaultBufSize, Nothing)] >>= newIORef
    rr <- newIORef $ RedisPubSub (defaultConnectInfo { connectHost = "127.0.0.1", connectPort = PortNumber 6379 })
    tr <- newIORef $ openJTalk settings
    br <- newIORef $ typeTalkBot settings
    wr <- let (WechimeSettings {..}) = wechime settings
          in initWechime gpio_dir gpio_chime1 gpio_chime2 gpio_chime12
    dr <- newResource $ let dbs = database settings in Database (PostgreSQL (dsn dbs) (max_connections dbs)) 

    return $ dr `RCons` rr `RCons` tr `RCons` br `RCons` wr `RCons` lr `RCons` RNil

app :: FilePath
    -> (AppSettings -> IO (Resources (Refs ResourceTypes)))
    -> IO Application
app config res = do
    s' <- decodeFileEither @AppSettings config
    settings <- case s' of 
        Right s -> return s
        Left e -> throw e

    resources <- res settings

    let contextTypes = Proxy :: Proxy '[ RequestContextEntry SiteKeys ResourceTypes
                                       , LinkSettings
                                       , CrossDomainOrigin
                                       , M.Map String VoiceProperties
                                       ]

    let rs = hoistServerWithContext (Proxy :: Proxy ResourceAPI)
                                    contextTypes
                                    (actionHandler resources)
                                    resourceServer

    let linkContext = Mintz.Settings.link settings
    let crossDomainContext = CrossDomainOrigin (origin $ cross_domain settings)
    let voiceProperties = Mintz.Settings.voices $ open_jtalk settings

    return $ resourceApp (Proxy :: Proxy AllAPI)
                         resources
                         (Proxy :: Proxy SiteKeys)
                         (linkContext :. crossDomainContext :. voiceProperties :. EmptyContext)
                         (rs :<|> serveDirectoryWebApp "public")
