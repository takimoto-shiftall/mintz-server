module Mintz.Resource.OpenJTalk where

import GHC.IO.Handle
import qualified Codec.Binary.UTF8.String as UTF8
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Exception.Safe
import Data.IORef
import qualified Data.Char as C
import Data.String
import Data.Maybe (maybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import System.Directory
import System.Process
import System.Exit
import System.FilePath (replaceExtension, (</>), (<.>))
import Crypto.Hash
import Data.Resource

defaultVoiceFile = "mei/mei_happy.htsvoice"

data OpenJTalk = OpenJTalk { dictDir :: String
                           , voiceDir :: String
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
              -> FilePath
              -> [String]
openJTalkArgs (OpenJTalkContext jt) voice p =
    [ "-x"
    , dictDir jt
    , "-m"
    , (voiceDir jt </> voice)
    , "-a", "0.5"
    , "-r", "0.9"
    , "-b", "0.2"
    , "-fm", "0.4"
    , "-ow", p
    ]

wavePath :: OpenJTalkContext
         -> String
         -> FilePath
wavePath c@(OpenJTalkContext jt) name = outDir jt </> name <.> "wav"

mp3Path :: FilePath
        -> FilePath
mp3Path p = replaceExtension p "mp3"

outputWave :: OpenJTalkContext
           -> Maybe String
           -> String
           -> String
           -> IO (Maybe FilePath)
outputWave c@(OpenJTalkContext jt) voice text name
    | isPronounceable text = do
        let path = wavePath c name
        let voice' = maybe defaultVoiceFile id voice
        ec <- withCreateProcess (proc "open_jtalk" (openJTalkArgs c voice' path)) { std_in = CreatePipe } $ \(Just inh) _ _ ph -> do
            let text' = B.pack (UTF8.encode text)
            hSetBuffering inh NoBuffering
            C8.hPutStrLn inh text'
            waitForProcess ph
        return $ if ec == ExitSuccess then Just path else Nothing
    | otherwise = return Nothing

waveToMP3 :: FilePath
          -> IO (Maybe FilePath)
waveToMP3 input = do
    let output = mp3Path input
    exists <- doesPathExist output
    ec <- withCreateProcess (proc "avconv" ["-y", "-i", input, output]) $ \_ _ _ ph -> do
        waitForProcess ph
    return $ if ec == ExitSuccess then Just output else Nothing

outputMP3 :: OpenJTalkContext
          -> Maybe String
          -> String
          -> String
          -> IO (Maybe FilePath)
outputMP3 c voice text name =
    runMaybeT $ MaybeT (outputWave c voice text name) >>= (MaybeT . (\p -> waveToMP3 p <* removeFile p))

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

execMP3 :: (With '[OpenJTalkContext])
        => Maybe String
        -> String
        -> FilePath
        -> Bool
        -> IO (Maybe FilePath)
execMP3 voice text name overwrite = do
    jtalk <- readIORef $ contextOf @OpenJTalkContext ?cxt
    let output = mp3Path $ wavePath jtalk name
    exists <- doesPathExist output
    if not exists || overwrite
        then do
            logCD ?cxt "Execute generation of sound files."
            outputMP3 jtalk voice text name 
        else do
            logCD ?cxt "File already exists. Sound generation is skipped"
            return $ Just output

isPronounceable :: String
                -> Bool
isPronounceable text = any pronounceable text
    where
        pronounceable :: Char -> Bool
        pronounceable c = and $ map ($ c) $ map (not .) [C.isControl, C.isSpace]