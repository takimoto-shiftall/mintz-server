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
import Servant.API
import Servant.Server
import Servant.Utils.StaticFiles
import Network.Wai (Application)
import Database.Redis
import Data.Resource
import Database.HDBC
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Data.Config
import Data.Validation
import Ext.Servant
import Mintz.Settings
import Mintz.HTTP.Site.Person
import Mintz.HTTP.API.Publish
import Mintz.HTTP.API.Person
import Mintz.HTTP.API.Voice
import Mintz.Resource.Redis
import Mintz.Resource.OpenJTalk
import Mintz.Resource.TypeTalk
import Mintz.Resource.Wechime

type ResourceAPI =  "site" :> ( PersonSite )
               :<|> "api" :> CrossDomain '[ 'GET, 'POST, 'PUT, 'DELETE, 'OPTIONS ]
                            :> ( PublishAPI
                            :<|> PersonAPI
                            :<|> VoiceAPI
                               )

type AllAPI = (AppResources @> AppKeys :> ResourceAPI)
         :<|> "public" :> Raw

resourceServer sc = personSite sc
               :<|> ( publishAPI sc
                 :<|> personAPI sc
                 :<|> voiceAPI sc
                    )

instance ResourceConfigurable Settings where
    type RC'Resources Settings = AppResources
    type RC'Contexts Settings = AppContexts

    getResources settings = do
        lr <- newLoggingResource [(anyTag, LevelDebug, LogStdout defaultBufSize, Nothing)] >>= newIORef
        rr <- newIORef $ RedisPubSub (defaultConnectInfo { connectHost = "127.0.0.1", connectPort = PortNumber 6379 })
        tr <- newIORef $ openJTalk
        br <- newIORef $ typeTalkBot
        wr <- let (Settings'wechime {..}) = wechime settings
              in initWechime gpio_dir (fromInteger gpio_chime1)
                                      (fromInteger gpio_chime2)
                                      (fromInteger gpio_chime12)
        dr <- newResource $ let dbs = database settings
                            in Database (PostgreSQL (dsn dbs) (fromInteger $ max_connections dbs)) 

        return $ dr `RCons` rr `RCons` tr `RCons` br `RCons` wr `RCons` lr `RCons` RNil
        where
            typeTalkBot = let tt = type_talk settings
                          in TypeTalkBot (token tt) (fromInteger $ topic_id tt)
            openJTalk = let jt = open_jtalk settings
                        in OpenJTalk { dictDir = dict_dir jt, voiceDir = voice_dir jt, outDir = out_dir jt }

    getContexts settings = do
        let linkContext = Mintz.Settings.link settings
        let crossDomainContext = CrossDomainOrigin (origin $ cross_domain settings)
        let voiceProperties = Mintz.Settings.voices $ open_jtalk settings

        return $ linkContext :. crossDomainContext :. voiceProperties :. EmptyContext

app :: FilePath
    -> IO (Either [ValidationError] (Application, Settings))
app config = do
    loadYamlFile @Settings' config >>= \s' -> do
        case s' of
            Left s -> return $ Left $ errorsOf s
            Right s -> do
                app <- configureResourceApp @AppKeys @ResourceAPI @AllAPI
                                            s
                                            resourceServer
                                            (:<|> serveDirectoryWebApp "public")
                return $ Right (app, s)