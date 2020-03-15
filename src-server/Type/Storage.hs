{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Type.Storage
    ( HashEBSD(..)
    , StorageBucket(bktText)
    , voxelBucket
    , facesBucket
    , edgesBucket
    , vertexBucket
    , ebsdBucket
    ) where

import GHC.Generics
import Data.Text (Text)

newtype HashEBSD = HashEBSD Text deriving (Show, Generic, Eq)

newtype StorageBucket = StorageBucket {bktText :: Text} deriving (Show, Generic, Eq)

voxelBucket :: StorageBucket
voxelBucket  = StorageBucket "voxel"

facesBucket :: StorageBucket
facesBucket  = StorageBucket "faces"

edgesBucket :: StorageBucket
edgesBucket  = StorageBucket "edges"

vertexBucket :: StorageBucket
vertexBucket = StorageBucket "vertex"

ebsdBucket :: StorageBucket
ebsdBucket = StorageBucket "ebsd"