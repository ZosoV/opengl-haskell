--REAME: Bindings to setup everything, and tie them to the callbacks.
module Graphics3D.Bindings where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad ( when, unless )
import System.Exit

-- Own Packages
import Graphics3D.Display
import Graphics3D.State
import Graphics3D.Utils

keyboard :: State -> KeyboardMouseCallback
keyboard state key keyState mods _ = do
   modifiers state $= mods
   postRedisplay Nothing
   case (key, keyState) of
      (Char 'r', Down) -> renderFullModel state $~ not
      (Char 'a', Down) -> showAxis state $~ not
      (Char 'b', Down) -> nextClearColor state
      (Char 'q', Down) -> exitWith ExitSuccess
      (Char '\27', Down) -> exitWith ExitSuccess
      (Char 't', Down) -> modelCycle state $~ tail
      (Char ' ', Down) -> toggleRotation state
      (Char '+', Down) -> theScale state $~ (+ scaleIncrement)
      (Char '-', Down) -> theScale state $~ (+ (- scaleIncrement))
      (Char _, Down) -> printHelp
      (SpecialKey KeyHome, Down) -> resetState state
      (SpecialKey KeyLeft, Down) -> diff state $~ ($- Vector3 1 0 0)
      (SpecialKey KeyRight, Down) -> diff state $~ ($+ Vector3 1 0 0)
      (SpecialKey KeyUp, Down) -> diff state $~ ($- Vector3 0 1 0)
      (SpecialKey KeyDown, Down) -> diff state $~ ($+ Vector3 0 1 0)
      (MouseButton LeftButton, Down) -> do
         inertia state $= pure 0
         lastIncr state $= pure 0
      (MouseButton LeftButton, Up) -> calcInertia state
      (_, _) -> return ()

reshape :: ReshapeCallback
reshape size@(Size w h) = do
    let vp = 0.8
        aspect = fromIntegral w / fromIntegral h

    viewport $= (Position 0 0, size)

    matrixMode $= Projection
    loadIdentity
    frustum (-vp) vp (-vp / aspect) (vp / aspect) 3 10

    matrixMode $= Modelview 0
    loadIdentity
    translate (Vector3 0 0 (-5 :: GLfloat))

motion :: State -> MotionCallback
motion state pos@(Position x y) = do
    postRedisplay Nothing
    Position xt yt <- get (lastPosition state)
    lastPosition state $= pos
    when (xt /= -1 || yt /= -1) $ do
        let li@(Vector3 xl yl _) = Vector3 (fromIntegral (x - xt)) (fromIntegral (y - yt)) 0
        lastIncr state $= li
        when (xt /= -1) $ do
            mods <- get (modifiers state)
            if ctrl mods == Down
                then do diff state $~ ($+ Vector3 0 0 xl)
                        theScale state $~ (+ (yl * scaleFactor))
                else diff state $~ ($+ li)
         
timer :: State -> TimerCallback
timer state = do
rot <- get (shouldRotate state)
when rot $ do
    ia <- get (inertia state)
    diff state $~ ($+ ia)
    postRedisplay Nothing
addTimerCallback timerFrequencyMillis (timer state)