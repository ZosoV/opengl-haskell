--- README: Define different display function according what you need
module Display (display) where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Graphics.GL.Internal.Shared
import Graphics.UI.GLUT.Fonts
import Data.IORef

-- Own Packages
import GraphicsUtil
import SamplePoints
import Axis
import Draw2D

-- Function that display the principal objects of the scene
display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef [Bool]-> DisplayCallback
display angle pos flags = do
    clear [ColorBuffer]
    loadIdentity
    -- load flags
    [show_labels, dual, separate] <- get flags

    drawAxis 1
    -- draw2DSurface my2DPoints
    drawAllCells my2DCells angle pos show_labels
    if dual then draw2DDual my2DDualPoints else return ()
    -- labelCells my2DCells
    swapBuffers

-- Draw the dual surface given a set of points
draw2DDual :: (Foldable t, VertexComponent a) => t (a, a, a) -> IO ()
draw2DDual points = do
    currentColor $= Color4 0 0 1 1
    renderPrimitive Lines $
        mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points
