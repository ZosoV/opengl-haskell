module Axis where

import Graphics.UI.GLUT

-- Function that draws a coordinate system in the scene
drawAxis :: IO ()
drawAxis = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
        vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
    renderPrimitive Lines $ do
        color3f 1 0 0
        vertex3f (-1) 0 0
        vertex3f 1 0 0

        color3f 0 1 0
        vertex3f 0 (-1) 0
        vertex3f 0 1 0

        color3f 0 0 1
        vertex3f 0 0 (-1)
        vertex3f 0 0 1