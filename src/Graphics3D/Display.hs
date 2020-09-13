--- README: Define different display function according what you need
module Graphics3D.Display where

import Graphics.UI.GLUT
import Graphics.GL.Internal.Shared

-- Own Packages
import Graphics3D.State
import Graphics3D.Models
import Axis

display :: State -> DisplayCallback
display state = do
   loadIdentity
   translate (Vector3 0 0 (-5 :: GLfloat))

   Vector3 xDiff yDiff zDiff <- get (diff state)
   rotate yDiff (Vector3 1 0 0)
   rotate xDiff (Vector3 0 1 0)
   rotate zDiff (Vector3 0 0 1)

   sc <- get (theScale state)
   scale sc sc sc

   clear [ ColorBuffer, DepthBuffer ]
   
   sAxis <- get (showAxis state)

   if sAxis 
      then do
      drawAxis 1.1
      drawAxisLabels 1.2
      else do
         return ()   
   
   rFull <- get (renderFullModel state)
   if rFull then do
         -- Draw Full Model
         drawFullModel cellsNormal
      else do
         -- Draw One Cell
         drawOneCell state

   flush
   swapBuffers

drawOneCell :: State -> IO()   
drawOneCell state = do
   -- Draw Solid Model
   ((draw,model_color):_) <- get (modelCycle state)
   renderWithColor draw model_color

   -- Draw Outline
   glPolygonMode GL_FRONT_AND_BACK GL_LINE
   renderWithColor draw $ Color3 (0::GLfloat) 0 0
   glPolygonMode GL_FRONT_AND_BACK GL_FILL

renderWithColor :: IO () -> Color3 GLclampf -> IO()
renderWithColor draw colorModel = do
   color $ colorModel
   draw