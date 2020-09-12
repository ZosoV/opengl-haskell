 --- README: Define a state type to control the changes in the render

module Graphics3D.State where

import Graphics.UI.GLUT
import Data.IORef

-- Own Packages
import Graphics3D.Models

clearColors :: [Color4 GLclampf]
clearColors = [
    Color4 0.0 0.0 0.0 1,
    Color4 0.2 0.2 0.3 1,
    Color4 0.7 0.7 0.7 1 ]

initialDiff :: Vector3 GLfloat
initialDiff = Vector3 206 16 10

initialInertia :: Vector3 GLfloat
initialInertia = Vector3 (-0.5) 0 0

data State = State {
   diff :: IORef (Vector3 GLfloat),
   lastIncr :: IORef (Vector3 GLfloat),
   inertia :: IORef (Vector3 GLfloat),
   inertiaOld :: IORef (Vector3 GLfloat),
   theScale :: IORef GLfloat,
   lastPosition :: IORef Position,
   shouldRotate :: IORef Bool,
   colorCycle :: IORef [Color4 GLclampf],
   modelCycle :: IORef [IO ()],
   modifiers :: IORef Modifiers
   }

makeState :: IO State
makeState = do
   di <- newIORef initialDiff
   li <- newIORef (pure 0)
   ia <- newIORef initialInertia
   io <- newIORef (pure 0)
   sc <- newIORef 1
   lp <- newIORef (Position (-1) (-1))
   sr <- newIORef False
   cc <- newIORef (cycle clearColors)
   mc <- newIORef (cycle models)
   mo <- newIORef (Modifiers Up Up Up)
   return $ State {
      diff = di,
      lastIncr = li,
      inertia = ia,
      inertiaOld = io,
      theScale = sc,
      lastPosition = lp,
      shouldRotate = sr,
      colorCycle = cc,
      modelCycle = mc,
      modifiers = mo
      }