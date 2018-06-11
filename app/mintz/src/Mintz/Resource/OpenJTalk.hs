module Mintz.Resource.OpenJTalk where

import GHC.IO.Handle
import qualified Codec.Binary.UTF8.String as UTF8
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Exception.Safe
import Data.IORef
import Data.String
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import System.Process
import Crypto.Hash
import Data.Resource

data OpenJTalk = OpenJTalk { dictDir :: String
                           , voiceFile :: String
                           , outDir :: String
                           }

data OpenJTalkContext = OpenJTalkContext OpenJTalk

-- open_jtalk \
-- -x ./open_jtalk_dic_utf_8-1.07/ \
-- -m /usr/local/Cellar/open-jtalk/1.10_1/voice/mei/mei_happy.htsvoice \
-- -a 0.5 \
-- -r 0.9 \
-- -b 0.2 \
-- -fm 0.4 \
-- -ow out.wav sample.txt

openJTalkArgs :: OpenJTalkContext
              -> FilePath
              -> [String]
openJTalkArgs (OpenJTalkContext jt) p =
    [ "-x"
    , dictDir jt
    , "-m"
    , voiceFile jt
    , "-a", "0.5"
    , "-r", "0.9"
    , "-b", "0.2"
    , "-fm", "0.4"
    , "-ow"
    , outDir jt ++ "/" ++ p ++ ".wav"
    ]

outputWave :: OpenJTalkContext
           -> String
           -> IO FilePath
outputWave c text = do
    let name = show $ hashWith SHA1 (fromString text :: B.ByteString)
    (Just inh, _, _, _) <- createProcess $ (proc "open_jtalk" (openJTalkArgs c name)) { std_in = CreatePipe }
    let text' = B.pack (UTF8.encode text)
    C8.hPutStrLn inh text'
    return name

instance Resource OpenJTalk where
    type ContextType OpenJTalk = OpenJTalkContext

    newContext ref = liftIO $ readIORef ref >>= newIORef . OpenJTalkContext 

instance ResourceContext OpenJTalkContext where
    type ResourceType OpenJTalkContext = OpenJTalk

    closeContext cxt status = return cxt

    -- RunInBase m IO == m a -> IO (StM m a)
    -- control :: (RunInBase m IO -> IO (StM m a)) -> m a
    --         == ((m a -> IO (StM m a)) -> IO (StM m a)) -> m a
    execContext ref action = action
