module Bindings (idle, display, reshape, keyboardMouse) where

import Graphics.UI.GLUT
import Data.IORef

-- Own Packages
import Display

reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef [Bool] -> KeyboardMouseCallback
keyboardMouse a p flags key  Down _ _ = case key of
  (Char '1') -> flags $~! \(l@(x:xs)) -> replaceNth 0 (not $ l!!0) l
  (Char '2') -> flags $~! \(l@(x:xs)) -> replaceNth 1 (not $ l!!1) l
  (Char ' ') -> a $~! negate
  (Char '+') -> a $~! (* 2)
  (Char '-') -> a $~! (/ 2)
  (SpecialKey KeyLeft ) -> p $~! \(x,y) -> (x-0.01,y)
  (SpecialKey KeyRight) -> p $~! \(x,y) -> (x+0.01,y)
  (SpecialKey KeyUp   ) -> p $~! \(x,y) -> (x,y+0.01)
  (SpecialKey KeyDown ) -> p $~! \(x,y) -> (x,y-0.01)
  _ -> return ()
keyboardMouse _ _ _ _ _ _ _ = return ()

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  -- angle $~! (+ d)
  postRedisplay Nothing

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs