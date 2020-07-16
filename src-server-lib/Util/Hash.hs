{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Hash
    ( calculateHashEBSD
    , calculateHashOR
    , calculateHashArche
    ) where

import Data.Hashable (hash)
import Data.Word     (Word)
import File.EBSD     (EBSDdata)
import Numeric       (showHex)

import qualified Data.Text as T

import qualified Arche.Strategy.ORFitAll as OR

import Type.Storage (HashEBSD(..), HashOR(..), HashArche(..))
import Type.Store   (ArcheCfg)
import Util.OrphanInstances ()

calculateHashEBSD :: EBSDdata -> HashEBSD 
calculateHashEBSD = HashEBSD . T.pack . (\(x :: Word) -> showHex x "") . fromIntegral . hash

calculateHashOR :: OR.Cfg -> HashOR 
calculateHashOR = HashOR . T.pack . (\(x :: Word) -> showHex x "") . fromIntegral . hash

calculateHashArche :: ArcheCfg -> HashArche 
calculateHashArche = HashArche . T.pack . (\(x :: Word) -> showHex x "") . fromIntegral . hash