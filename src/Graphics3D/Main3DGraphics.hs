-- README: Function to render the main loop
module Graphics3D.Main3DGraphics where

import Graphics.UI.GLUT
import Data.IORef
import Control.Exception ( IOException, catch )

-- Own Packages
import Graphics3D.Bindings
import Graphics3D.State
import Graphics3D.Display
import Graphics3D.Utils

plot2 :: IO ()
plot2 = do
   _ <- getArgsAndInitialize
   
   -- Init the modes of the windows, in this case consider a 3D version
   initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
   
   -- Set the window size and name it
   initialWindowSize $= Size 500 500
   _ <- createWindow "Tropical Plot"

   -- Note: We don't use an idle callback, we redisplay more intelligently.

   -- create a state data type which control the changes of the draw
   state <- makeState


   displayCallback $= display state
   keyboardMouseCallback $= Just (keyboard state)
   reshapeCallback $= Just reshape
   
   -- Registers a motion callback. The motion callback for a window is called 
   -- when the mouse moves within the window while one or more mouse buttons 
   -- are pressed.
   motionCallback $= Just (motion state)

   -- Registers a timer callback to be triggered in a specified number of milliseconds.
   -- addTimerCallback timerFrequencyMillis (timer state)

   -- Define the Global Settings of the Pipeline
   putStrLn "Using fixed function pipeline."
--    materialDiffuse Front $= Color4 1 0.3 0.2 1
--    materialSpecular Front $= Color4 0.3 0.3 0.3 1
--    materialShininess Front $= 16
--    position (Light 0) $= Vertex4 0 0 4 0
--    position (Light 0) $= Vertex4 1 0.4 0.8 1
--    lighting $= Enabled
--    light (Light 0) $= Enabled

   depthFunc $= Just Less -- the comparison function for depth the buffer
   nextClearColor state

   -- display help
   keyboard state (Char '?') Down (Modifiers Up Up Up) (Position 0 0)

   mainLoop