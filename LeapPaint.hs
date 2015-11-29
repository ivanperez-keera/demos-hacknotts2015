{-# LANGUAGE MultiWayIf #-}
import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Monad (when, void)
import Data.Aeson
import Data.IORef
import Data.Maybe
import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import GHC.Float
import Graphics.UI.SDL            as FX
import Graphics.UI.SDL.Image      as FX
import Graphics.UI.SDL.Types      as FX
import Graphics.UI.SDL.Primitives as FX
import Graphics.UI.SDL.Rotozoomer as FX
import System.Console.ANSI

import Leap
import Leap.Hand      as Hand
import Leap.Gesture
import Leap.Pointable hiding (length)
import Leap.BaseTypes (Vec3, Mat3)

data DrawingTool = Brush
                 | Fingers
                 | Palm
                 | None
 deriving Eq

data DrawingSettings = DrawingSettings
 { drawingTool   :: !DrawingTool
 , activated     :: ![(Int, Int)]
 , toolColor     :: !(Int, Int, Int, Int)
 , drops         :: ![(Int, Int, Int, Int, Int, Int, Int)]
 , rotatingLeft  :: !Bool
 , rotatingRight :: !Bool
 , greenMore     :: !Bool
 , greenLess     :: !Bool
 , redMore       :: !Bool
 , redLess       :: !Bool
 , blueMore      :: !Bool
 , blueLess      :: !Bool
 , alphaMore     :: !Bool
 , alphaLess     :: !Bool
 }

defaultDrawingSettings :: DrawingSettings
defaultDrawingSettings = DrawingSettings
  { drawingTool   = Brush
  , activated     = []
  , toolColor     = (128, 255, 255, 255)
  , drops         = []
  , rotatingLeft  = False
  , rotatingRight = False
  , greenMore     = False
  , greenLess     = False
  , redMore       = False
  , redLess       = False
  , blueMore      = False
  , blueLess      = False
  , alphaMore     = False
  , alphaLess     = False
  }

updateTool :: DrawingSettings -> IO DrawingSettings
updateTool ds = do
  ev <- FX.pollEvent
  case ev of
    FX.NoEvent -> return ds
    -- Movement
    FX.KeyDown (Keysym SDLK_a _ _) -> return $ ds { redMore       = True }
    FX.KeyUp   (Keysym SDLK_a _ _) -> return $ ds { redMore       = False }
    FX.KeyDown (Keysym SDLK_z _ _) -> return $ ds { redLess       = True }
    FX.KeyUp   (Keysym SDLK_z _ _) -> return $ ds { redLess       = False }
    FX.KeyDown (Keysym SDLK_s _ _) -> return $ ds { greenMore     = True }
    FX.KeyUp   (Keysym SDLK_s _ _) -> return $ ds { greenMore     = False }
    FX.KeyDown (Keysym SDLK_x _ _) -> return $ ds { greenLess     = True }
    FX.KeyUp   (Keysym SDLK_x _ _) -> return $ ds { greenLess     = False }
    FX.KeyDown (Keysym SDLK_d _ _) -> return $ ds { blueMore      = True }
    FX.KeyUp   (Keysym SDLK_d _ _) -> return $ ds { blueMore      = False }
    FX.KeyDown (Keysym SDLK_c _ _) -> return $ ds { blueLess      = True }
    FX.KeyUp   (Keysym SDLK_c _ _) -> return $ ds { blueLess      = False }
    FX.KeyDown (Keysym SDLK_f _ _) -> return $ ds { alphaMore     = True }
    FX.KeyUp   (Keysym SDLK_f _ _) -> return $ ds { alphaMore     = False }
    FX.KeyDown (Keysym SDLK_v _ _) -> return $ ds { alphaLess     = True }
    FX.KeyUp   (Keysym SDLK_v _ _) -> return $ ds { alphaLess     = False }
    FX.KeyUp   (Keysym SDLK_q _ _) -> return $ ds { drawingTool   = Brush }
    FX.KeyUp   (Keysym SDLK_w _ _) -> return $ ds { drawingTool   = Fingers }
    FX.KeyUp   (Keysym SDLK_e _ _) -> return $ ds { drawingTool   = None }
    FX.KeyUp   (Keysym SDLK_g _ _) -> return $ ds { rotatingLeft  = False }
    FX.KeyDown (Keysym SDLK_g _ _) -> return $ ds { rotatingLeft  = True }
    FX.KeyUp   (Keysym SDLK_b _ _) -> return $ ds { rotatingRight = False }
    FX.KeyDown (Keysym SDLK_b _ _) -> return $ ds { rotatingRight = True }
    FX.KeyUp   (Keysym SDLK_r _ _)
      -> return $ ds { drops = drops ds ++
                               [(x,y,151,r,g,b,a) | let (r,g,b,a) = toolColor ds
                                                  , (x,y) <- activated ds
                                                  ] }
    -- Anything else
    _ -> updateTool ds

adjustColorComponent :: Int -> Int
adjustColorComponent c
  | c > 255   = 255
  | c < 0     = 0
  | otherwise = c


-- updateDrawing :: [Vec3] -> DrawingSettings -> DrawingSettings
-- updateDrawing vs ds = ds { activated = mapMaybe paintingTips vs }

updateDrawing leapInfo ds =
 updateColor $ updateDrops $ updateDrawing' leapInfo ds

updateColor ds = ds { toolColor = (r',g',b',a') }
 where (r,g,b,a) = toolColor ds
       (r',g',b',a') = ( adjustColorComponent $ redF r
                       , adjustColorComponent $ greenF g
                       , adjustColorComponent $ blueF b
                       , adjustColorComponent $ alphaF a)
       redF   = if | redMore ds   -> (+2)
                   | redLess ds   -> (\x -> x - 2)
                   | otherwise    -> Prelude.id
       greenF = if | greenMore ds -> (+2)
                   | greenLess ds -> (\x -> x - 2)
                   | otherwise    -> Prelude.id
       blueF  = if | blueMore ds  -> (+2)
                   | blueLess ds  -> (\x -> x - 2)
                   | otherwise    -> Prelude.id
       alphaF = if | alphaMore ds -> (+2)
                   | alphaLess ds -> (\x -> x - 2)
                   | otherwise    -> Prelude.id

updateDrawing' leapInfo ds
 | drawingTool ds == Brush
 = ds { activated =  map leapToScreenCoordinates
                            $ mapMaybe (paintingTips . stabilizedTipPosition) (pointables leapInfo) }
 | drawingTool ds == Fingers
 = ds { activated =  map leapToScreenCoordinates
                            $ mapMaybe paintingTips $ map palmPosition (hands leapInfo) }
 | otherwise
 = ds { activated = [] }

paintingTips :: Vec3 -> Maybe (Float, Float)
paintingTips (x,y,z) = Just (x,z)

updateDrops ds =
 ds { drops = [(x,y,e,r,g,b,a) | (x,y',e',r,g,b,a) <- drops ds
                               , let e = e' - 1
                               , let y = y' + 1
                               , e' > 0] }

-- * Main program
main :: IO ()
main = do
 -- Video visualiser: initialisation
 FX.init [InitVideo]
 FX.setVideoMode width height 32 [HWSurface]
 canvas <- FX.getVideoSurface

 -- FX.getVideoSurface
 let format = FX.surfaceGetPixelFormat canvas
 white <- FX.mapRGB format 0xFF 0xFF 0xFF
 FX.fillRect canvas Nothing white

 drawingSettings <- newMVar defaultDrawingSettings

 -- Processing
 runLeap defaultConfig $ \leapInfo -> do
   -- print (pointables x)

   modifyMVar_ drawingSettings updateTool
   settings <- modifyReadMVar drawingSettings (updateDrawing leapInfo)

   let rotation = if | rotatingLeft settings  -> (-1)
                     | rotatingRight settings -> 1
                     | otherwise              -> 0

   when (rotation /= 0) $ void $ do
          copy <- FX.createRGBSurface [SWSurface] width height 32
                     0x000000FF 0x0000FF00 0x00FF0000 0xFF000000
          FX.fillRect copy Nothing white
          FX.blitSurface canvas (Just $ Rect 0 0 width (height - 70))
                         copy Nothing
          rotatedScreen <- FX.rotozoom copy rotation 1 True
          let (c1x, c1y) = (width `div` 2, height `div` 2)
              (w2 , h2 ) = (surfaceGetWidth rotatedScreen, surfaceGetHeight rotatedScreen)
              (c2x, c2y) = (w2 `div` 2, h2 `div` 2)
              (dx, dy)   = (c2x - c1x, c2y - c1y)
          -- print (dx, dy)
          FX.fillRect canvas Nothing white
          FX.blitSurface rotatedScreen (Just $ Rect 0 0 w2 h2)
                         canvas        (Just $ Rect (-dx) (-dy) width height)

   let paintAction = case drawingTool settings of
                  Brush   -> \(x,y) -> void $ filledCircle
                                 canvas
                                 (fromIntegral x)
                                 (fromIntegral y)
                                 10
                                 (colorToPixel $ toolColor settings)
                  Fingers -> \(x,y) -> void $ filledCircle
                                 canvas
                                 (fromIntegral x)
                                 (fromIntegral y)
                                 20
                                 (colorToPixel $ toolColor settings)
                  None    -> \_ -> return ()
   mapM_ paintAction (activated settings)

   let paintDrop (x,y,e,r,g,b,a) = void $ filledCircle
                                           canvas
                                           (fromIntegral x)
                                           (fromIntegral y)
                                           (max 16 (fromIntegral e `div` 5))
                                           (colorToPixel (r,g,b,a))

   print (drops settings)

   mapM_ paintDrop (drops settings)

   FX.fillRect canvas (Just $ Rect 0 (height - 70) width 70) white

   let (r,g,b) = (\(r,g,b,a) -> (fromIntegral r,fromIntegral g,fromIntegral b)) (toolColor settings)
   curColor <- FX.mapRGB format r g b
   FX.fillRect canvas (Just $ Rect (width - 60) (height - 60) 50 50) curColor

   FX.flip canvas

leapToScreenCoordinates (x,y) = ( width  `div` 2 + round (x / 200 * width  / 2)
                                , height `div` 2 + round (y / 200 * height / 2))

colorToPixel :: (Int, Int, Int, Int) -> Pixel
colorToPixel (r,g,b,a) = Pixel (rw .|. gw .|. bw .|. aw)
 where rw = shift ((fromIntegral r) :: Word32) 24
       gw = shift ((fromIntegral g) :: Word32) 16
       bw = shift ((fromIntegral b) :: Word32)  8
       aw = (fromIntegral r) :: Word32

-- * IORef: modify and return the new value
modifyReadMVar :: MVar a -> (a -> a) -> IO a
modifyReadMVar ref f = modifyMVar ref (f >>> \x -> return (x,x))

width :: Num a => a
width = 1024

height :: Num a => a
height = 768
