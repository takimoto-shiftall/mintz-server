{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

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
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options (provideOptions)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
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

type ResourceAPI = (@>) '[DBResource Database, RedisPubSub, OpenJTalk, TypeTalkBot, Wechime, LoggingResource] SiteKeys
                    :> ( "site"
                        :> ( PersonSite
                           )
                    :<|> "api"
                        -- :> CrossDomain '[ 'GET, 'POST, 'PUT, 'DELETE, 'OPTIONS ]
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

policy :: CorsResourcePolicy
policy = simpleCorsResourcePolicy {
      corsRequestHeaders = ["*"]
    , corsMethods = ["*"]
    , corsOrigins = Nothing
    }

main :: IO ()
main = do
    s' <- decodeFileEither @AppSettings "config/develop.yml"
    settings <- case s' of 
        Right s -> return s
        Left e -> throw e

    lr <- newLoggingResource [(anyTag, LevelDebug, LogStdout defaultBufSize, Nothing)] >>= newIORef
    rr <- newIORef $ RedisPubSub (defaultConnectInfo { connectHost = "127.0.0.1", connectPort = PortNumber 6379 })
    tr <- newIORef $ openJTalk settings
    br <- newIORef $ typeTalkBot settings
    wr <- let (WechimeSettings {..}) = wechime settings
          in initWechime gpio_dir gpio_chime1 gpio_chime2 gpio_chime12
    dr <- newResource $ let dbs = database settings in Database (PostgreSQL (dsn dbs) (max_connections dbs)) 

    let resources = dr `RCons` rr `RCons` tr `RCons` br `RCons` wr `RCons` lr `RCons` RNil

    let contextTypes = Proxy :: Proxy '[ RequestContextEntry SiteKeys '[ DBResource Database
                                                                       , RedisPubSub
                                                                       , OpenJTalk
                                                                       , TypeTalkBot
                                                                       , Wechime
                                                                       , LoggingResource
                                                                       ]
                                       , LinkSettings
                                       , CrossDomainOrigin
                                       , M.Map String VoiceProperties
                                       ]

    let rs = hoistServerWithContext (Proxy :: Proxy ResourceAPI)
                                    contextTypes
                                    (actionHandler resources)
                                    resourceServer
    --let rs = enter (NT $ actionHandler resources) resourceServer

    let linkContext = Mintz.Settings.link settings
    let crossDomainContext = CrossDomainOrigin (origin $ cross_domain settings)
    let voiceProperties = Mintz.Settings.voices $ open_jtalk settings

    let app = resourceApp (Proxy :: Proxy AllAPI)
                          resources
                          (Proxy :: Proxy SiteKeys)
                          (linkContext :. crossDomainContext :. voiceProperties :. EmptyContext)
                          (rs :<|> serveDirectoryWebApp "public")

    -- Warp.run 8001 $ cors (const $ Just policy) $ provideOptions (Proxy :: Proxy AllAPI) $ app
    Warp.run 8001 app