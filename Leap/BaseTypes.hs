{-# LANGUAGE DeriveGeneric #-}

module Leap.BaseTypes where

import GHC.Generics
import Data.Aeson

type Vec3 = (Float, Float, Float)
type Mat3 = (Vec3, Vec3, Vec3)

data InteractionBox = InteractionBox {
  center :: Vec3,
  size :: Vec3 
} deriving (Show, Generic)

instance FromJSON InteractionBox
instance ToJSON InteractionBox
