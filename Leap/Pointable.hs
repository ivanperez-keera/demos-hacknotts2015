{-# LANGUAGE DeriveGeneric #-}
module Leap.Pointable where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import Leap.BaseTypes (Vec3, Mat3)

data LeapPointable = LeapPointable{
  direction :: Vec3,
  handId :: Integer,
  id :: Integer,
  length :: Float,
  stabilizedTipPosition :: Vec3,
  timeVisible :: Float,
  tipPosition :: Vec3,
  tipVelocity :: Vec3,
  tool :: Bool,
  touchDistance :: Float,
  touchZone :: Text
} deriving (Show, Generic)

instance FromJSON LeapPointable
instance ToJSON LeapPointable

