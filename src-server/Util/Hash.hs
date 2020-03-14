{-# LANGUAGE RecordWildCards #-}

module Util.Hash
    ( calculateHashEBSD
    ) where

import Data.Hashable (hash)
import File.EBSD     (EBSDdata)
import Numeric       (showHex)

import qualified Data.Text as T

import Type.Storage (HashEBSD(..))
import Util.OrphanInstances ()

calculateHashEBSD :: EBSDdata -> HashEBSD 
calculateHashEBSD = HashEBSD . T.pack . flip showHex "" . hash