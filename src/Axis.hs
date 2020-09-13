module Axis where

import Graphics.UI.GLUT
import GraphicsUtil

-- Function that draws a coordinate system in the scene
drawAxis :: GLfloat -> IO ()
drawAxis size = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
        vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
    renderPrimitive Lines $ do
        color3f 1 0 0
        vertex3f (-size) 0 0
        vertex3f size 0 0


        color3f 0 1 0
        vertex3f 0 (-size) 0
        vertex3f 0 size 0

        color3f 0 0 1
        vertex3f 0 0 (-size)
        vertex3f 0 0 size

drawAxisLabels :: GLfloat -> IO ()        
drawAxisLabels size = do
    let colorx  = Color3 1 0 0
        x_minus = Vector3 (-size) 0 0
        x_plus  = Vector3 size 0 0
        
        colory  = Color3 0 1 0
        y_minus = Vector3 0 (-size) 0
        y_plus  = Vector3 0 size 0

        colorz  = Color3 0 0 1
        z_minus = Vector3 0 0 (-size)
        z_plus  = Vector3 0 0 size

    renderLabel x_minus colorx "-x"
    renderLabel x_plus colorx "x"
    renderLabel y_minus colory "-y"
    renderLabel y_plus colory "y"
    renderLabel z_minus colorz "-z"
    renderLabel z_plus colorz "z"

renderLabel :: Vector3 GLfloat -> Color3 GLfloat -> String -> IO ()
renderLabel position color_label label = do
    preservingMatrix $ do
        color $ color_label
        translate $ position   
        scale 0.0005 0.0005 (0.00005::GLfloat) -- Scale of characters
        renderString Roman label