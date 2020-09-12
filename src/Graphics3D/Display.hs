--- README: Define different display function according what you need
module Graphics3D.Display where

import Graphics.UI.GLUT

-- Own Packages
import Graphics3D.State

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
   (drawModel:_) <- get (modelCycle state)
   drawModel

   -- models!!1
   -- models!!2
   -- models!!3

   flush
   swapBuffers