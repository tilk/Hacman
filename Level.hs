{-# LANGUAGE NoMonomorphismRestriction #-}
module Level(
    Fld(..), Ghost(..), Level(..), Dot(..), 
    levelDimX, levelDimY, emptyLevel, readLevel, loadLevel, levelWrap,
    levelForGhosts,
    Dir(..), dirCase, dirToX, dirToY, dirToXY, dirIsV, dirIsH, oppositeDir) where

import Data.Array.IArray
import Data.Array.Unboxed
import Control.Arrow
import Control.Monad
import Control.Monad.State

data Fld = FldEmpty | FldWall | FldTWall | FldGWall deriving (Eq, Enum, Show)
data Ghost = GBlinky | GPinky | GInky | GClyde deriving (Eq, Enum, Show)
data Dot = DSmall | DBig deriving (Eq, Enum, Show)

data Level = Level {
        levelPacPos :: (Int, Int),
        levelGhosts :: [((Int, Int), Ghost)],
        levelDots :: [((Int, Int), Dot)],
        levelDimensions :: (Int, Int),
        levelRespawn :: (Int, Int),
        levelMap :: (Int, Int) -> Fld
    } 

emptyLevel = Level { 
    levelPacPos = (0, 0), 
    levelGhosts = [], 
    levelDots = [], 
    levelDimensions = (0, 0), 
    levelRespawn = (0, 0), 
    levelMap = undefined 
}

levelDimX = fst . levelDimensions
levelDimY = snd . levelDimensions

readLevel s = level { levelDimensions = (xsize, ysize), levelMap = \i -> toEnum $ mapArray ! i }
    where
    (mapData, level) = runState (mapM (uncurry scanLine) $ zip [0..] $ reverse $ lines s) emptyLevel 
    xsize = maximum (map length mapData)
    ysize = length mapData
    mapArray = array ((-1, -1), (xsize, ysize)) $ map (id *** fromEnum) (concat mapData) :: UArray (Int, Int) Int
    scanLine y l = mapM (uncurry $ flip scanChar y) $ zip [0..] l
    scanChar x y c = case c of
        ' ' -> xreturn FldEmpty
        '*' -> xreturn FldWall
        '#' -> xreturn FldTWall
        '%' -> xreturn FldGWall
        '.' -> addDot DSmall x y >> xreturn FldEmpty
        'o' -> addDot DBig   x y >> xreturn FldEmpty
        'P' -> modify (\i -> i { levelPacPos = (x, y) }) >> xreturn FldEmpty
        '@' -> modify (\i -> i { levelRespawn = (x, y) }) >> xreturn FldEmpty
        'A' -> addGhost GBlinky x y >> xreturn FldEmpty
        'B' -> addGhost GPinky  x y >> xreturn FldEmpty
        'C' -> addGhost GInky   x y >> xreturn FldEmpty
        'D' -> addGhost GClyde  x y >> xreturn FldEmpty
        where xreturn a = return ((x,y),a)
    addGhost g x y = modify (\i -> i { levelGhosts = ((x, y), g):levelGhosts i })
    addDot g x y = modify (\i -> i { levelDots = ((x, y), g):levelDots i })

loadLevel path = fmap readLevel (readFile path)

levelWrap level (x,y) | x < 0 = levelWrap level (x+levelDimX level, y)
                      | y < 0 = levelWrap level (x, y+levelDimY level)
                      | x >= levelDimX level = levelWrap level (x-levelDimX level, y)
                      | y >= levelDimY level = levelWrap level (x, y-levelDimY level)
                      | otherwise = (x,y)

levelForGhosts level = level { levelMap = dropGWall . levelMap level } where
    dropGWall FldGWall = FldEmpty
    dropGWall f = f

data Dir = DirL | DirR | DirU | DirD | DirC deriving (Eq, Enum, Show)

dirCase l r u d c dir = [l, r, u, d, c] !! fromEnum (dir :: Dir)
dirToX = dirCase (-1) 1 0 0 0
dirToY = dirCase 0 0 1 (-1) 0
dirToXY = dirToX &&& dirToY

dirIsH d = d == DirL || d == DirR
dirIsV d = d == DirU || d == DirD

oppositeDir DirL = DirR
oppositeDir DirR = DirL
oppositeDir DirU = DirD
oppositeDir DirD = DirU
oppositeDir DirC = DirC

