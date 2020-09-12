--- README: scripts where we define the models to be render
module Graphics3D.Models where

import Graphics.UI.GLUT

models :: [IO ()]
models = [
   drawCube,
   drawCell $ my3DCells!!0,
   drawCell $ my3DCells!!1,
   drawCell $ my3DCells!!2,
   renderObject Solid (Torus 0.2 0.6 64 64) ]

drawFace :: Normal3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat
   -> Vertex3 GLfloat -> Vertex3 GLfloat -> IO ()
drawFace p q r s t = do
    let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
    normal p
    texCoord2f (TexCoord2 1 1)
    vertex q
    texCoord2f (TexCoord2 0 1)
    vertex r
    texCoord2f (TexCoord2 0 0)
    vertex s
    texCoord2f (TexCoord2 1 0)
    vertex t

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

toNorm3 :: [a] -> Normal3 a
toNorm3 v@([x,y,z]) = Normal3 x y z

toVector3 :: [a] -> Vector3 a
toVector3 v@([x,y,z]) = Vector3 x y z

drawCell :: [[GLfloat]] -> IO()
drawCell points = do
    renderPrimitive Triangles $ mapM_ vertex3f points 


drawCube :: IO ()
drawCube = do
   let size = 1
       sc = 0.2
       delta = 0.1

       a = Vertex3   size    size  ( size * sc + delta)
       b = Vertex3   size    size  (-size * sc + delta)
       c = Vertex3   size  (-size) (-size * sc)
       d = Vertex3   size  (-size) ( size * sc)
       e = Vertex3 (-size)   size  ( size * sc + delta)
       f = Vertex3 (-size)   size  (-size * sc + delta)
       g = Vertex3 (-size) (-size) (-size * sc)
       h = Vertex3 (-size) (-size) ( size * sc)

       i = Normal3   1    0    0
       k = Normal3 (-1)   0    0
       l = Normal3   0    0  (-1)
       m = Normal3   0    0    1
       n = Normal3   0    1    0
       o = Normal3   0  (-1)   0

   renderPrimitive Quads $ do
      drawFace i d c b a
      drawFace k g h e f
      drawFace l c g f b
      drawFace m h d a e
      drawFace n e a b f
      drawFace o g c d h