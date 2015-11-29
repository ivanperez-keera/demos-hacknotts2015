{-# LANGUAGE OverloadedStrings, DeriveGeneric  #-}
module Leap where

import Leap.Hand (LeapHand)
import Leap.Pointable (LeapPointable)
import Leap.Gesture (LeapGesture)

import GHC.Generics
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Data.Aeson
import Control.Applicative
import Control.Monad (mzero)
import Leap.BaseTypes (Vec3, Mat3, InteractionBox)

data LeapEvent = LeapEvent {
  currentFrameRate :: Float,
  id :: Integer,
  interactionBox :: InteractionBox,
  timestamp :: Integer,
  r :: (Vec3, Vec3, Vec3),
  s :: Float,
  t :: Vec3,
  hands :: [LeapHand],
  pointables :: [LeapPointable],
  gestures :: [LeapGesture]
} deriving (Show, Generic)

instance FromJSON LeapEvent;
instance ToJSON LeapEvent;

data LeapVersion = LeapVersion {
  version :: Integer
}

instance FromJSON LeapVersion where
  parseJSON (Object v) = LeapVersion <$> v .: "version"
  parseJSON _ = mzero


type LeapHandler = LeapEvent -> IO ()

data LeapConfig = LeapConfig {
  host :: String,
  port :: Int
}

defaultConfig :: LeapConfig
defaultConfig = LeapConfig {
  host = "localhost",
  port = 6437
}

runLeap :: LeapConfig -> LeapHandler -> IO ()
runLeap config handler = WS.runClient (host config) (port config) "/" (leapApp handler)

leapApp handler conn = do
    versionMessage <- WS.receiveData conn
    liftIO $ case eitherDecode versionMessage of
             (Right (LeapVersion 1)) -> return ()
             (Right (LeapVersion i)) -> error $ "Unrecognised version " ++ (show i)
             (Left msg) -> error msg
    forever $ do
        msg <- WS.receiveData conn
        let result = eitherDecode msg
        case result of
          (Left e) -> error e
          (Right a) -> liftIO $ handler a
