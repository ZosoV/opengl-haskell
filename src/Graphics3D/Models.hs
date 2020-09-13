--- README: scripts where we define the models to be render
module Graphics3D.Models where

import Graphics.UI.GLUT
import Graphics.GL.Internal.Shared
import Control.Monad

type GLVertex = [GLfloat]
type GLCell = [GLVertex]

modelColors :: [Color3 GLclampf]   
modelColors = [
    Color3 1.000 0.494 0.494 ,
    Color3 0.482 0.922 0.722 ,
    Color3 0.220 0.851 0.831 ,
    Color3 0.533 0.596 0.965 ,
    Color3 0.992 0.706 0.416 ,
    Color3 0.627 0.722 0.878 ,
    Color3 0.925 0.965 0.576 ,
    Color3 0.976 0.714 0.886 ]

models :: [[([GLVertex], GLVertex)]] -> [(IO (),Color3 GLclampf)]
models cells = zip ioDrawCells colorPoints
    where len = length cells
          colorPoints = take len modelColors 
          ioDrawCells = map drawCell cells

cellsNormal :: [[([GLVertex], GLVertex)]]
cellsNormal =   [
                [( [[0,0,0],[0,0,1],[0,1,0]] , [-1,0,0] ),
                ( [[0,0,0],[0,0,1],[1,1,1]] , [1,-1,0] ),
                ( [[0,0,0],[0,1,0],[1,1,1]] , [1,0,-1] ),
                ( [[0,0,1],[0,1,0],[1,1,1]] , [-1,1,1] )]
                ,
                [( [[0,0,0],[0,0,1],[1,0,0]] , [0,-1,0] ),
                ( [[0,0,0],[0,0,1],[1,1,1]] , [-1,1,0] ),
                ( [[0,0,0],[1,0,0],[1,1,1]] , [0,1,-1] ),
                ( [[0,0,1],[1,0,0],[1,1,1]] , [1,-1,1] )]
                ,
                [( [[0,0,0],[0,1,0],[1,0,0]] , [0,0,-1] ),
                ( [[0,0,0],[0,1,0],[1,1,1]] , [-1,0,1] ),
                ( [[0,0,0],[1,0,0],[1,1,1]] , [0,-1,1] ),
                ( [[0,1,0],[1,0,0],[1,1,1]] , [1,1,-1] )]
                ]

my3DCells :: [[[GLfloat]]]
my3DCells = [[[0,0,0],[0,0,1],[0,1,0],
              [0,0,0],[0,0,1],[1,1,1],
              [0,0,0],[0,1,0],[1,1,1],
              [0,0,1],[0,1,0],[1,1,1]]
              ,
              [[0,0,0],[0,0,1],[1,0,0],
              [0,0,0],[0,0,1],[1,1,1],
              [0,0,0],[1,0,0],[1,1,1],
              [0,0,1],[1,0,0],[1,1,1]]
              ,
              [[0,0,0],[0,1,0],[1,0,0],
              [0,0,0],[0,1,0],[1,1,1],
              [0,0,0],[1,0,0],[1,1,1],
              [0,1,0],[1,0,0],[1,1,1]]]

vertex3f :: [GLfloat] -> IO ()
vertex3f [x, y, z] = vertex $ Vertex3 x y z

normal3f :: [GLfloat] -> IO ()
normal3f [x, y, z] = normal $ Normal3 x y z

toNorm3 :: [a] -> Normal3 a
toNorm3 v@([x,y,z]) = Normal3 x y z

toVector3 :: [a] -> Vector3 a
toVector3 v@([x,y,z]) = Vector3 x y z

toColor3 :: [a] -> Color3 a
toColor3 v@([x,y,z]) = Color3 x y z

drawFace' :: ([GLVertex], GLVertex) -> IO()
drawFace' (face,norm) = do
    normal3f norm
    vertex3f (face !! 0)
    normal3f norm
    vertex3f (face !! 1)
    normal3f norm
    vertex3f (face !! 2)

drawCell :: [([GLVertex], GLVertex)] -> IO()
drawCell cell = do
    renderPrimitive Triangles $ mapM_ drawFace' cell 

drawCellWithColor :: ([([GLVertex], GLVertex)], Color3 GLclampf) -> IO()
drawCellWithColor (cell,color_cell) = do
    color $ color_cell
    renderPrimitive Triangles $ mapM_ drawFace' cell 

drawCellWithColor' :: Color3 GLclampf -> [([GLVertex], GLVertex)]  -> IO()
drawCellWithColor' color_cell cell = do
    color $ color_cell
    renderPrimitive Triangles $ mapM_ drawFace' cell 

drawFullModel :: [[([GLVertex], GLVertex)]] -> IO()
drawFullModel cells = do
    let cell_colors = zip cells modelColors
    mapM_ drawCellWithColor cell_colors
    glPolygonMode GL_FRONT_AND_BACK GL_LINE
    mapM_ ( drawCellWithColor' $ Color3 (0::GLfloat) 0 0 ) cells
    glPolygonMode GL_FRONT_AND_BACK GL_FILL
