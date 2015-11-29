{-# LANGUAGE OverloadedStrings  #-}
module Leap.Gesture where

import Data.Text (Text)
import Data.Aeson
import Leap.BaseTypes (Vec3, Mat3)
import Control.Monad (mzero)
import Control.Applicative

data LeapGesture = LeapGesture{
  direction :: Maybe Vec3,
  duration :: Float,
  handIds :: [Integer],
  id' :: Integer,
  pointableIds :: [Integer],
  position :: Maybe Vec3,
  speed :: Maybe Float,
  startPosition :: Maybe Vec3,
  state :: Text,
  type' :: Text
} deriving (Show)

instance FromJSON LeapGesture where 
  parseJSON (Object v) = LeapGesture <$> 
    v .:? "direction" <*>
    v .: "duration" <*>
    v .: "handIds" <*>
    v .: "id" <*>
    v .: "pointableIds" <*>
    v .:? "position" <*>
    v .:? "speed" <*>
    v .:? "startPosition" <*>
    v .: "state" <*>
    v .: "type"
  parseJSON _ = mzero

instance ToJSON LeapGesture where
   toJSON g = object [
      "direction" .= direction g,
      "duration" .= duration g,
      "handIds" .= handIds g,
      "id" .= id' g,
      "pointableIds" .= pointableIds g,
      "position" .= position g,
      "speed" .= speed g,
      "startPosition" .= startPosition g,
      "state" .= state g,
      "type" .= type' g
    ]
