module Mintz.Resource.OpenJTalk where

import GHC.IO.Handle
import qualified Codec.Binary.UTF8.String as UTF8
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Exception.Safe
import Data.IORef
import Data.String
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import System.Directory
import System.Process
import System.Exit
import System.FilePath (replaceExtension, (</>), (<.>))
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
    , "-ow", p
    ]

outputWave :: OpenJTalkContext
           -> String
           -> IO (Maybe FilePath)
outputWave c@(OpenJTalkContext jt) text = do
    let name = outDir jt </> (show $ hashWith SHA1 (fromString text :: B.ByteString)) <.> "wav"
    ec <- withCreateProcess (proc "open_jtalk" (openJTalkArgs c name)) { std_in = CreatePipe } $ \(Just inh) _ _ ph -> do
        let text' = B.pack (UTF8.encode text)
        hSetBuffering inh NoBuffering
        C8.hPutStrLn inh text'
        waitForProcess ph
    return $ if ec == ExitSuccess then Just name else Nothing

waveToMP3 :: FilePath
          -> IO (Maybe FilePath)
waveToMP3 input = do
    let output = replaceExtension input "mp3"
    ec <- withCreateProcess (proc "avconv" ["-y", "-i", input, output]) $ \_ _ _ ph -> do
        waitForProcess ph
    return $ if ec == ExitSuccess then Just output else Nothing

outputMP3 :: OpenJTalkContext
          -> String
          -> IO (Maybe FilePath)
outputMP3 c text = runMaybeT $ MaybeT (outputWave c text) >>= (MaybeT . (\p -> waveToMP3 p <* removeFile p))

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
