{-# LANGUAGE DeriveGeneric #-}
module Leap.Hand where

import GHC.Generics
import Data.Aeson
import Leap.BaseTypes (Vec3, Mat3)

data LeapHand = LeapHand {
  direction :: Vec3,
  id :: Integer,
  palmNormal :: Vec3,
  palmVelocity :: Vec3,
  palmPosition :: Vec3,
  r :: Mat3,
  s :: Float,
  t :: Vec3,
  sphereRadius :: Float,
  sphereCenter :: Vec3,
  stabilizedPalmPosition :: Vec3,
  timeVisible :: Float
} deriving (Show, Generic)

instance FromJSON LeapHand;
instance ToJSON LeapHand;
