module MainGraphics where

import Graphics.UI.GLUT
import Data.IORef

-- Own Packages
import Bindings


-- Main function that run the windonw and OpenGL mainLoop
plot :: IO ()
plot = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    _window <- createWindow "Tropical Geometry"
    windowSize $= Size 900 900
    clearColor $= Color4 0.8 0.95 1 1
    reshapeCallback $= Just reshape
    angle <- newIORef 0
    delta <- newIORef 0.1
    pos <- newIORef (0, 0)
    flags <- newIORef [True,False,False] -- (show_label,show_dual,separate)
    keyboardMouseCallback $= Just (keyboardMouse delta pos flags)
    idleCallback $= Just (idle angle delta)
    displayCallback $= display angle pos flags
    mainLoop    

