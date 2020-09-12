module Draw2D where

import Graphics.UI.GLUT
import Graphics.GL.Internal.Shared
import Control.Monad
import Data.IORef (IORef)
import GraphicsUtil
import Data.List (zip4)

-- Own Packages
import SamplePoints

vertex3f :: GLVertex -> IO ()
vertex3f [x, y, z] = vertex $ Vertex3 x y z

toVector3 :: [a] -> Vector3 a
toVector3 v@([x,y,z]) = Vector3 x y z

-- Draw only one cell with an specific label
drawOneCell :: [GLVertex] -> GLVertex -> String -> Bool -> IO ()
drawOneCell cell centroid label show_labels = do 
    renderPrimitive Triangles $ mapM_ vertex3f cell 
    if show_labels then renderLabel centroid label else return ()


renderLabel :: GLVertex -> String -> IO ()
renderLabel position label = do
    translate $ toVector3 position   
    scale 0.0005 0.0005 (0.0005::GLfloat) -- Scale of characters
    renderString Roman label

-- Draw all cells 
drawAllCells :: [GLCell] -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> Bool-> DisplayCallback
drawAllCells cells angle pos show_labels = do
    (x',y') <- get pos
    -- translate $ Vector3 0.0 0.0 (0.0::GLfloat)
    preservingMatrix $ do
        a <- get angle
        rotate a $ Vector3 0 0 1
        currentColor $= Color4 0 0 0 1
        glPolygonMode GL_FRONT_AND_BACK GL_LINE
        forM_ info_zip $ \ v@(cell, direction,centroid,label) ->
            preservingMatrix $ do
                translate $ toVector3 $ map (*x') direction
                drawOneCell cell centroid label show_labels
        glPolygonMode GL_FRONT_AND_BACK GL_FILL
    where
        centroids = map getCentroid cells
        directions = getNormalizedDirectionVectors cells myExtremalVertexs
        labels = createLabels $ length cells
        info_zip = zip4 cells directions centroids labels

-- checkOutBounds 
