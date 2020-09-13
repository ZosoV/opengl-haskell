module Graphics3D.Utils where

import Graphics.UI.GLUT
import Control.Applicative
import Control.Monad ( when, unless )

-- Own Packages
import Graphics3D.State

infixl 6 $+, $-
infixl 7 $*

inertiaThreshold, inertiaFactor :: GLfloat
inertiaThreshold = 1
inertiaFactor = 0.5

scaleFactor, scaleIncrement :: GLfloat
scaleFactor = 0.01
scaleIncrement = 0.5

timerFrequencyMillis :: Timeout
timerFrequencyMillis = 20

-- Our tiny vector math library...
($+), ($-), ($*) :: (Applicative t, Num a) => t a -> t a -> t a
($+) = liftA2 (+)
($-) = liftA2 (-)
($*) = liftA2 (*)

step :: (Applicative t, Num a, Ord a) => t a -> t a -> t a
step = liftA2 (\e x -> if x < e then 0 else 1)

dot :: (Applicative t, Foldable t, Num a) => t a -> t a -> a
dot v1 v2 = sum (v1 $* v2)

nextClearColor :: State -> IO ()
nextClearColor state = do
    cc <- get (colorCycle state)
    clearColor $= head cc
    colorCycle state $~ tail

toggleRotation :: State -> IO ()
toggleRotation state = do
    rot <- get (shouldRotate state)
    shouldRotate state $~ not
    if rot
        then do
            ia <- get (inertia state)
            inertiaOld state $= ia
        else do
        io <- get (inertiaOld state)
        inertia state $= io
        -- To prevent confusion, force some rotation
        when (dot io io == 0) $
            inertia state $= initialInertia

printHelp :: IO ()
printHelp = mapM_ putStrLn [
    "",
    "KEYBOARD COMMANDS:",
    "",
    "a \t \t \t \t - Show axis",
    "r \t \t \t \t - Display full model (all cells).",
    "t \t \t \t \t - Toggle among cells to render",
    "b \t \t \t \t - Toggle among background clear colors",
    "q or <esc> \t \t \t - Quit",
    "? \t \t \t \t - Help",
    "<home> \t \t \t \t - reset zoom and rotation",
    "<space> or <click> \t \t - stop rotation",
    "<+>, <-> or <ctrl + drag> \t - zoom model",
    "<arrow keys> or <drag> \t \t - rotate model",
    ""]

resetState :: State -> IO ()
resetState state = do
    diff state $= initialDiff
    lastIncr state $= pure 0
    inertia state $= initialInertia
    theScale state $= 1

calcInertia :: State -> IO ()
calcInertia state = do
    lastPosition state $= Position (-1) (-1)
    li <- get (lastIncr state)
    ia <- get (inertia state)
    let t = pure inertiaThreshold
        f = pure inertiaFactor
        l = (pure 1 $- (step (fmap negate t) li)) $* ((li $+ t) $* f $- ia)
        r = (step t li) $* ((li $- t) $* f $- ia)
    inertia state $= l $+ ia $+ r
    lastIncr state $= pure 0

