module SamplePoints where

import Graphics.UI.GLUT
import GraphicsUtil

-- Points that defines the 2D cells of the tropical curve
-- Each tree points define a triangle
my2DCells :: [GLCell]
my2DCells  = [[[-0.4,  0,   0], [-0.4, 0.4, 0], [ 0,    0,   0]],
            [[-0.4,  0,   0], [ 0,   0,   0], [-0.4, -0.4, 0]],
            [[-0.4, -0.4, 0], [ 0,   0,   0], [ 0,   -0.4, 0]],
            [[ 0,   -0.4, 0], [ 0,   0,   0], [ 0.4, -0.4, 0]]]

-- Points that defines the dual
my2DDualPoints :: [(GLfloat,GLfloat,GLfloat)]
my2DDualPoints = [(-0.56,  0.13,   0),(-0.26,  0.13,   0),
                  (-0.26, -0.13,  0) , (-0.13, -0.26,  0),
                  (-0.13, -0.26,  0) , (-0.13, -0.56,  0),
                  (-0.13, -0.26,  0) , (0.13, -0.26, 0),
                  (-0.26,  0.13,   0),( 0,   0.3,   0),
                  (-0.26,  0.13,   0),(-0.26,  -0.13,   0),
                  (-0.26, -0.13,  0) ,(-0.56, -0.13, 0),
                  (0.13, -0.26, 0) , (0.13, -0.56, 0),
                  (0.13, -0.26, 0) , (0.43, 0, 0)
                  ]              
                  
myCellPoints :: [(GLfloat,GLfloat,GLfloat)]
myCellPoints =  [(-0.26,  0.13,   0), (-0.26, -0.13,  0), (-0.13, -0.26,  0), (0.13, -0.26, 0)]



myExtremalVertexs :: [GLVertex]
myExtremalVertexs = [[-0.4, 0.4, 0],[-0.4, -0.4, 0],[ 0.4,   -0.4, 0]]

-- Example of a Tropical surface defined by its polyhedrons.
-- my3DPolyhedrons :: [[[GLfloat]]]
-- my3DPolyhedrons = [[[0,0,0],[0,0,1],[0,1,0]
--                     [0,0,0],[0,0,1],[1,1,1]
--                     [0,0,0],[0,1,0],[1,1,1]
--                     [0,0,1],[0,1,0],[1,1,1]]
--                     ,
--                     [[0,0,0],[0,0,1],[1,0,0]
--                     [0,0,0],[0,0,1],[1,1,1]
--                     [0,0,0],[1,0,0],[1,1,1]
--                     [0,0,1],[1,0,0],[1,1,1]]
--                     ,
--                     [[0,0,0],[0,1,0],[1,0,0]
--                     [0,0,0],[0,1,0],[1,1,1]
--                     [0,0,0],[1,0,0],[1,1,1]
--                     [0,1,0],[1,0,0],[1,1,1]]]