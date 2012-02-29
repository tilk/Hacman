{-# LANGUAGE RankNTypes, ViewPatterns #-}
module LevelPath(levelPath, emptyPath, forwardDirs, awayDirs) where

import Level
import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.NumInstances

type Path = UArray (Int, Int) Int

qMin (h:t, r) = Just (h, (t, r))
qMin ([], []) = Nothing
qMin ([], r) = qMin (reverse r, [])

qInsert e (t, r) = (t, e:r)

qEmpty = ([], [])

emptyPath :: Level -> Path
emptyPath level = runSTUArray $ (newArray ((-1,-1), levelDimensions level) maxBound :: forall s. ST s (STUArray s (Int, Int) Int))

levelPath :: Level -> (Int, Int) -> Path
levelPath level start = runSTUArray $ do
    dirinfo <- newArray ((-1,-1), levelDimensions level) maxBound :: forall s. ST s (STUArray s (Int, Int) Int)
    dstep dirinfo (qInsert (0, start) qEmpty)
    return dirinfo
    where
    dstep dirinfo (qMin -> Just ((k,p), h)) 
        | levelMap level p /= FldEmpty = dstep dirinfo h
        | otherwise = do
        ak <- readArray dirinfo p
        if ak <= k then dstep dirinfo h
          else do
            writeArray dirinfo p k
            dstep dirinfo $ foldr (\p -> qInsert (k+1, levelWrap level p)) h [p+(1,0),p+(-1,0),p+(0,1),p+(0,-1)]
    dstep _ _ = return ()

pathDirs :: (Int -> Int -> Bool) -> Level -> Path -> (Int, Int) -> [Dir]
pathDirs rel level path pos
    | path ! pos == maxBound = []
    | otherwise = [DirL, DirR, DirU, DirD] >>= go where
        go dir = guard (v < maxBound && v `rel` (path ! pos)) >> return dir
            where v = path ! (levelWrap level $ dirToXY dir + pos)

forwardDirs = pathDirs (<)

awayDirs = pathDirs (>)

