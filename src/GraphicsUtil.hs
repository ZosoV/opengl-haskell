module GraphicsUtil where

import Graphics.UI.GLUT
import qualified Data.Vector as V

-- Data types

type GLVertex = [GLfloat]
type GLCell = [GLVertex]

safeZipWithV :: (a -> b -> c) -> [a] -> [b] -> [c]
safeZipWithV f vector1 vector2
    | length vector1 /= length vector2 = error "Vectors must have the same size for safe zipping"
    | otherwise = zipWith f vector1 vector2

mapV :: (a -> b) -> [[a]] -> [[b]]
mapV f vertex_list= map (map f) vertex_list

getCentroid :: [GLVertex] -> GLVertex
getCentroid set = map (/len) (foldr1 (safeZipWithV (+)) set)
    where len = fromIntegral $ length set 

-- getNormalizedDirectionVector current_position target 
getNormalizedDirectionVector :: GLVertex -> GLVertex -> GLVertex
getNormalizedDirectionVector [x1,y1,z1] [x2, y2, z2] =  map (/ norm) direction_vector
    where
        direction_vector = safeZipWithV (-) [x2,y2,z2] [x1,y1,z1]
        norm = foldl (+) 0 $ map (** 2) direction_vector

getNormalizedDirectionVectors :: [GLCell] -> [GLVertex] -> [GLVertex]
getNormalizedDirectionVectors cells extremal_vertexs = normalized_direction_vectors
    where
        global_centroid = getCentroid extremal_vertexs
        cells_centroids = map getCentroid cells
        normalized_direction_vectors = map (getNormalizedDirectionVector global_centroid) cells_centroids


createLabels :: (Eq t, Num t, Show t) => t -> [String]
createLabels len
    | len == 1 = [show 1]
    | otherwise = [show len] ++ createLabels (len - 1)

