-- Function to render the main loop
module Graphics3D.Main3DGraphics where

import Graphics.UI.GLUT
import Data.IORef
import Control.Exception ( IOException, catch )

-- Own Packages
import Graphics3D.Bindings
import Graphics3D.State
import Graphics3D.Display
import Graphics3D.Utils

plot3 :: IO ()
plot3 = do
   _ <- getArgsAndInitialize
   initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
   initialWindowSize $= Size 500 500
   _ <- createWindow "3Dlabs Brick Shader"

   -- Note: We don't use an idle callback, we redisplay more intelligently.
   state <- makeState
   displayCallback $= display state
   keyboardMouseCallback $= Just (keyboard state)
   reshapeCallback $= Just reshape
   motionCallback $= Just (motion state)
   addTimerCallback timerFrequencyMillis (timer state)

   putStrLn "Using fixed function pipeline."
   materialDiffuse Front $= Color4 1 0.3 0.2 1
   materialSpecular Front $= Color4 0.3 0.3 0.3 1
   materialShininess Front $= 16
   position (Light 0) $= Vertex4 0 0 4 0
   lighting $= Enabled
   light (Light 0) $= Enabled

   depthFunc $= Just Less
   nextClearColor state

   -- display help
   keyboard state (Char '?') Down (Modifiers Up Up Up) (Position 0 0)

   mainLoop