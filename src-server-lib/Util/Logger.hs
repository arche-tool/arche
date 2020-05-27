{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Util.Logger
    ( LogMessage(..)
    , ToLogMessage(..)
    , LogEntry (..)
    , LogLevel (..)
    , printLog
    , logEntry
    , logMsg
    -- * log on google monad  
    , logGoogle
    , logGGInfo
    , logGGError
    ) where

import Control.Lens                       (view)
import Control.Monad.IO.Class             (liftIO)
import Control.Monad.Representable.Reader (reader)
import Data.Aeson                         (ToJSON(..))
import Data.Binary.Builder                (Builder)
import Data.Time.Clock                    (UTCTime, getCurrentTime)
import GHC.Generics

import qualified Data.Aeson                   as A
import qualified Data.Aeson.Encoding.Internal as A
import qualified Data.Binary.Builder          as B
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Char8   as BSC
import qualified Network.Google               as Google

-- ============== Log message ================

class ToLogMessage a where
  toLogMsg :: a -> LogMessage 
  
  default toLogMsg :: ToJSON a => a -> LogMessage
  toLogMsg = LogMessage . A.fromEncoding . toEncoding

instance {-# OVERLAPPING #-} ToLogMessage LogMessage where
  toLogMsg = id

instance ToJSON a => ToLogMessage a

instance {-# OVERLAPPING #-} ToLogMessage BS.ByteString where
  toLogMsg = LogMessage . B.fromByteString

instance {-# OVERLAPPING #-} ToLogMessage BSL.ByteString where
  toLogMsg = LogMessage . B.fromLazyByteString

newtype LogMessage = LogMessage {logMsgBuilder :: Builder}

instance ToJSON LogMessage where
  toJSON = error "you should not use this unimplemented function but toEncoding"
  toEncoding (LogMessage builder) = let
    !cleaned = B.fromLazyByteString . mconcat . BSC.split '"' . B.toLazyByteString $ builder
    in A.unsafeToEncoding $ dquoteBuilder <> cleaned <> dquoteBuilder

dquoteBuilder :: Builder
dquoteBuilder = B.putCharUtf8 '"'

-- ============== Log level ================
data LogLevel
  = INFO
  | ERROR
  | WARN
  | DEBUG
  deriving Generic

instance ToJSON LogLevel where
  toEncoding = A.genericToEncoding A.defaultOptions

-- ============== Log entry ================
data LogEntry
  = LogEntry
  { message   :: !LogMessage
  , timestamp :: !UTCTime
  , level     :: !LogLevel
} deriving (Generic)

instance ToJSON LogEntry where
  toEncoding = A.genericToEncoding A.defaultOptions

printLogEntry :: LogEntry -> IO ()
printLogEntry = BSC.putStrLn . B.toLazyByteString . A.fromEncoding . toEncoding

logEntry :: LogLevel -> LogMessage -> IO LogEntry
logEntry l msg = do
  now <- getCurrentTime
  return $ LogEntry
    { level = l
    , message = toLogMsg msg
    , timestamp = now
    }

printLog :: LogLevel -> LogMessage -> IO ()
printLog l msg = logEntry l msg >>= printLogEntry

-- ================== Google Monad log ===================
logGoogle :: LogLevel -> LogMessage -> Google.Google s ()
logGoogle l x = do
  lg <- liftIO $ logEntry l x
  logger <- reader (view Google.envLogger)
  liftIO $ logger gglevel (A.fromEncoding . toEncoding $ lg)
  where
    gglevel = case l of
      INFO  -> Google.Info
      ERROR -> Google.Error
      WARN  -> Google.Info
      DEBUG -> Google.Debug

logGGInfo :: LogMessage -> Google.Google s ()
logGGInfo = logGoogle INFO

logGGError :: LogMessage -> Google.Google s ()
logGGError = logGoogle ERROR

-- ================== Polyvariadic log builder ===================
class LogMsgBuilder r where
  polyLogBuilder :: Bool -> Builder -> r

instance LogMsgBuilder LogMessage where
  polyLogBuilder _ = LogMessage

instance (ToLogMessage a, LogMsgBuilder r) => LogMsgBuilder (a -> r) where
  polyLogBuilder isend s c
    | isend     = polyLogBuilder False (logMsgBuilder (toLogMsg c))
    | otherwise = polyLogBuilder False (s <> spaceBuilder <> logMsgBuilder (toLogMsg c))

spaceBuilder :: Builder
spaceBuilder = B.putCharUtf8  ' '

logMsg :: (LogMsgBuilder r) => r
logMsg = polyLogBuilder True mempty