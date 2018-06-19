{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.IORef
import Control.Monad.Logger
import System.Log.FastLogger
import Data.Proxy
import qualified Data.Vault.Lazy as V
import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import Database.Redis
import Data.Resource
import Database.HDBC
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Ext.Servant.Action
import Ext.Servant.Context
import Mintz.Settings
import Mintz.HTTP.Site.Person
import Mintz.HTTP.API.Publish
import Mintz.HTTP.API.Person
import Mintz.Resource.Redis
import Mintz.Resource.OpenJTalk
import Mintz.Resource.TypeTalk

type ResourceAPI = (@>) '[DBResource Database, RedisPubSub, OpenJTalk, TypeTalkBot, LoggingResource] SiteKeys
                    :> ( "site"
                        :> ( PersonSite
                           )
                    :<|> "api"
                        :> ( PublishAPI
                        :<|> PersonAPI
                           )
                       )

type AllAPI = ResourceAPI
         :<|> "public" :> Raw

resourceServer sc = personSite sc
               :<|> ( publishAPI sc
                 :<|> personAPI sc
                    )

main :: IO ()
main = do
    lr <- newLoggingResource [(anyTag, LevelDebug, LogStdout defaultBufSize, Nothing)] >>= newIORef
    rr <- newIORef $ RedisPubSub (defaultConnectInfo { connectHost = "127.0.0.1", connectPort = PortNumber 6379 })
    tr <- newIORef openJTalk
    br <- newIORef typeTalk
    dr <- newResource db

    let resources = dr `RCons` rr `RCons` tr `RCons` br `RCons` lr `RCons` RNil

    let contextTypes = Proxy :: Proxy '[RequestContextEntry SiteKeys '[DBResource Database, RedisPubSub, OpenJTalk, TypeTalkBot, LoggingResource]]

    let rs = hoistServerWithContext (Proxy :: Proxy ResourceAPI)
                                    contextTypes
                                    (actionHandler resources)
                                    resourceServer
    --let rs = enter (NT $ actionHandler resources) resourceServer

    Warp.run 8001 $ resourceApp (Proxy :: Proxy AllAPI)
                                resources
                                (Proxy :: Proxy SiteKeys)
                                EmptyContext
                                (rs :<|> serveDirectoryWebApp "public")